%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.library_install.m.
%
% Build targets which install libraries.
%
%---------------------------------------------------------------------------%

:- module make.library_install.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % install_library(ProgressStream, Globals, MainModuleName, Succeeded,
    %   !Info, !IO)
    %
:- pred install_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % install_library_grade(LinkSucceeded0, MainModuleName, AllModuleNames,
    %   ProgressStream, Globals, Grade, Succeeded, !Info, !IO)
    %
    % NOTE The unusual argument order is required by the fact that
    % make.build.m calls this pred through foldl2_maybe_stop_at_error_loop.
    %
:- pred install_library_grade(maybe_succeeded::in,
    module_name::in, list(module_name)::in, io.text_output_stream::in,
    globals::in, string::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.compute_grade.
:- import_module libs.copy_util.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.clean.
:- import_module make.find_local_modules.
:- import_module make.get_module_dep_info.
:- import_module make.options_file.
:- import_module make.program_target.   % for make_misc_target
:- import_module make.timestamp.
:- import_module make.util.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

install_library(ProgressStream, Globals, MainModuleName, Succeeded,
        !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
        DepsSucceeded, AllModuleNames0, !Info, !IO),
    AllModuleNames = set.to_sorted_list(AllModuleNames0),
    make_install_dirs(ProgressStream, Globals,
        DirSucceeded, LinkSucceeded, !IO),
    ( if
        DepsSucceeded = succeeded,
        DirSucceeded = succeeded
    then
        list.map_foldl2(
            install_ints_and_headers(ProgressStream, Globals, LinkSucceeded),
            AllModuleNames, IntsSucceeded, !Info, !IO),
        install_extra_headers(ProgressStream, Globals,
            ExtraHdrsSucceeded, !IO),

        grade_directory_component(Globals, Grade),
        install_library_grade_files(ProgressStream, Globals, LinkSucceeded,
            Grade, MainModuleName, AllModuleNames, GradeSucceeded, !Info, !IO),
        ( if
            and_list([ExtraHdrsSucceeded | IntsSucceeded]) = succeeded,
            GradeSucceeded = succeeded
        then
            KeepGoing = make_info_get_keep_going(!.Info),
            % XXX With Mmake, LIBGRADES is target-specific.
            globals.lookup_accumulating_option(Globals, libgrades, LibGrades0),
            LibGrades = list.delete_all(LibGrades0, Grade),
            foldl2_install_library_grades(KeepGoing,
                LinkSucceeded, MainModuleName, AllModuleNames,
                ProgressStream, Globals, LibGrades, Succeeded, !Info, !IO)
        else
            Succeeded = did_not_succeed
        )
    else
        Succeeded = did_not_succeed
    ).

:- pred install_ints_and_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_ints_and_headers(ProgressStream, Globals, SubdirLinkSucceeded,
        ModuleName, Succeeded, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        % We always install the `.int0' files for a library even though they
        % are only required by the `.opt' files. This is because when building
        % a program with --intermodule-optimization enabled, the compiler will
        % look for `.int0' files of any libraries the program uses. It will do
        % this even for libraries that were not installed with
        % --intermodule-optimization enabled, returning an error if it cannot
        % find the `.int0' file.
        module_dep_info_get_children(ModuleDepInfo, Children),
        ( if set.is_empty(Children) then
            ExtExtDirs0 = []
        else
            ExtExtDirs0 = [{ext_cur_ngs(ext_cur_ngs_int_int0), "int0s"}]
        ),
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            ExtExtDirs1 =
                [{ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain),
                "opts"} | ExtExtDirs0]
        ;
            AnyIntermod = no,
            ExtExtDirs1 = ExtExtDirs0
        ),
        ExtExtDirs = [{ext_cur_ngs(ext_cur_ngs_int_int1), "ints"},
            {ext_cur_ngs(ext_cur_ngs_int_int2), "int2s"},
            {ext_cur_ngs(ext_cur_ngs_int_int3), "int3s"},
            {ext_cur_ngs_gs(ext_cur_ngs_gs_misc_module_dep), "module_deps"}
            | ExtExtDirs1],
        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix / "lib" / "mercury",
        list.map_foldl(
            install_subdir_file(ProgressStream, Globals, SubdirLinkSucceeded,
                LibDir / "ints", ModuleName),
            ExtExtDirs, Results, !IO),

        globals.get_target(Globals, Target),
        (
            % `.mh' files are (were) only generated for modules containing
            % `:- pragma foreign_export' declarations.
            % But `.mh' files are expected by Mmake so always generate them,
            % otherwise there is trouble using libraries installed by
            % `mmc --make' with Mmake.
            % XXX If we ever phase out mmake we could revert this behaviour.
            Target = target_c,
            % XXX Should we test
            % ModuleDepInfo ^ contains_foreign_export
            %   = contains_foreign_export?
            module_name_to_file_name(Globals, $pred,
                ext_cur_ngs_max_cur(ext_cur_ngs_max_cur_mh),
                ModuleName, FileName),
            install_file(ProgressStream, Globals, FileName, LibDir / "inc",
                HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(ProgressStream, Globals, SubdirLinkSucceeded,
                LibDir / "ints", ModuleName,
                {ext_cur_ngs_max_cur(ext_cur_ngs_max_cur_mh), "mhs"},
                HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            ( Target = target_java
            ; Target = target_csharp
            ),
            HeaderSucceeded = succeeded
        ),
        Succeeded = and_list([HeaderSucceeded | Results])
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed
    ).

:- pred install_extra_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_extra_headers(ProgressStream, Globals, ExtraHdrsSucceeded, !IO) :-
    globals.lookup_accumulating_option(Globals, extra_library_header,
        ExtraHdrs),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    IncDir = Prefix / "lib" / "mercury" / "inc",
    list.foldl2(install_extra_header(ProgressStream, Globals, IncDir),
        ExtraHdrs, succeeded, ExtraHdrsSucceeded, !IO).

:- pred install_extra_header(io.text_output_stream::in, globals::in,
    dir_name::in, string::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

install_extra_header(ProgressStream, Globals, IncDir, FileName,
        !Succeeded, !IO) :-
    install_file(ProgressStream, Globals, FileName, IncDir,
        InstallSucceeded, !IO),
    !:Succeeded = !.Succeeded `and` InstallSucceeded.

%---------------------------------------------------------------------------%

install_library_grade(LinkSucceeded0, MainModuleName, AllModuleNames,
        ProgressStream, Globals, Grade, Succeeded, !Info, !IO) :-
    % Only remove grade-dependent files after installing if
    % --use-grade-subdirs is not specified by the user.
    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        ( SubdirSetting = use_cur_dir
        ; SubdirSetting = use_cur_ngs_subdir
        ),
        CleanAfter = yes
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        CleanAfter = no
    ),

    EnvVarArgs = make_info_get_env_var_args(!.Info),
    % Set up so that grade-dependent files for the current grade
    % don't overwrite the files for the default grade.
    OptionArgs0 = make_info_get_option_args(!.Info),
    OptionArgs = OptionArgs0 ++ ["--grade", Grade, "--use-grade-subdirs"],

    verbose_make_two_part_msg(Globals, "Installing grade", Grade, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),

    lookup_mmc_options(make_info_get_options_variables(!.Info), MaybeMCFlags),
    (
        MaybeMCFlags = ok1(MCFlags),
        get_default_options(Globals, DefaultOptionTable),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        AllFlags = DetectedGradeFlags ++ MCFlags ++ EnvVarArgs ++ OptionArgs,
        handle_given_options(ProgressStream, DefaultOptionTable, AllFlags,
            _, _, OptionsSpecs, LibGlobals, !IO)
    ;
        MaybeMCFlags = error1(LookupSpecs),
        write_error_specs(ProgressStream, Globals, LookupSpecs, !IO),
        % Errors should have been caught before.
        unexpected($pred, "bad DEFAULT_MCFLAGS")
    ),

    (
        OptionsSpecs = [_ | _],
        usage_errors(ProgressStream, Globals, OptionsSpecs, !IO),
        Succeeded = did_not_succeed
    ;
        OptionsSpecs = [],

        % Remove the grade-dependent targets from the status map
        % (we need to rebuild them in the new grade).
        %
        % NOTE This code was disabled from 2008 jul 14 until 2023 dec 12
        % due to a bug in version_hash_table.delete. That bug, which was
        % due to the holes left by deletes in open addressing probe sequences,
        % was fixed by switching to separate chaining on 2009 mar 26.
        % The replacement code was
        %
        %   StatusMap = version_hash_table.init_default(dependency_file_hash)
        %
        % NOTE that each delete made by remove_target_file_if_grade_dependent
        % will create a new version_hash_table, even though, with the exception
        % of the last one, none of them can never be referred to again.
        % It is not clear whether keeping all non-grade-dependent files'
        % statuses in the map makes paying this cost worthwhile.
        %
        % XXX One obvious solution to this problem would be to
        % - convert StatusMap0 to an assoc list;
        % - delete all grade dependent files' entries from this assoc list;
        % - then construct a new version hash table from the result.
        StatusMap0 = make_info_get_dep_file_status_map(!.Info),
        version_hash_table.fold(remove_target_file_if_grade_dependent,
            StatusMap0, StatusMap0, StatusMap),

        make_info_set_dep_file_status_map(StatusMap, !Info),
        make_info_set_option_args(OptionArgs, !Info),

        % Reset the target file timestamp cache, as the information it contains
        % is not valid for the changed grade and grade-subdir setting.
        make_info_set_target_file_timestamps(init_target_file_timestamps,
            !Info),

        % Building the library in the new grade is done in a separate process
        % to make it easier to stop and clean up on an interrupt.
        globals.lookup_bool_option(LibGlobals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        call_in_forked_process(
            install_library_grade_2(ProgressStream, LibGlobals, LinkSucceeded0,
                MainModuleName, AllModuleNames, !.Info, CleanAfter),
            Succeeded0, !IO),
        Cleanup = maybe_make_grade_clean(ProgressStream, LibGlobals,
            CleanAfter, MainModuleName, AllModuleNames),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded0, Succeeded, !Info, !IO)
    ).

:- pred remove_target_file_if_grade_dependent(dependency_file::in,
    dependency_status::in,
    version_hash_table(dependency_file, dependency_status)::in,
    version_hash_table(dependency_file, dependency_status)::out) is det.

remove_target_file_if_grade_dependent(File, _Status, !StatusMap) :-
    ( if
        File = dep_target(target_file(_, TargetType)),
        % XXX Why are we deleting arch-dependent target types
        % that are NOT grade dependent?
        target_is_grade_or_arch_dependent(TargetType)
    then
        version_hash_table.delete(File, !StatusMap)
    else
        true
    ).

:- pred install_library_grade_2(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, module_name::in, list(module_name)::in, make_info::in,
    bool::in, maybe_succeeded::out, io::di, io::uo) is det.

install_library_grade_2(ProgressStream, Globals, LinkSucceeded0,
        MainModuleName, AllModuleNames, Info0, CleanAfter, Succeeded, !IO) :-
    make_misc_target(ProgressStream, Globals,
        MainModuleName - misc_target_build_library, LibSucceeded,
        Info0, Info1, [], Specs, !IO),
    (
        LibSucceeded = succeeded,
        % `GradeDir' differs from `Grade' in that it is in canonical form.
        grade_directory_component(Globals, GradeDir),
        install_library_grade_files(ProgressStream, Globals, LinkSucceeded0,
            GradeDir, MainModuleName, AllModuleNames, Succeeded,
            Info1, Info2, !IO),
        maybe_make_grade_clean(ProgressStream, Globals, CleanAfter,
            MainModuleName, AllModuleNames, Info2, _Info, !IO)
    ;
        LibSucceeded = did_not_succeed,
        % XXX MAKE_STREAM
        io.output_stream(ErrorStream, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO),
        Succeeded = did_not_succeed
    ).

    % Install the `.a', `.so', `.jar', `.opt' and `.mih' files for
    % the current grade.
    %
:- pred install_library_grade_files(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, string::in, module_name::in, list(module_name)::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_library_grade_files(ProgressStream, Globals, LinkSucceeded0, GradeDir,
        MainModuleName, AllModuleNames, Succeeded, !Info, !IO) :-
    make_grade_install_dirs(ProgressStream, Globals, GradeDir,
        DirResult, LinkSucceeded1, !IO),
    % TODO {ext_cur_ngs_gs(ext_cur_ngs_gs_misc_module_dep), "module_deps"}
    LinkSucceeded = LinkSucceeded0 `and` LinkSucceeded1,
    (
        DirResult = succeeded,
        globals.get_target(Globals, Target),
        get_std_grade_specific_install_lib_dir(Globals, GradeDir, GradeLibDir),
        (
            Target = target_csharp,
            linked_target_file_name(Globals, MainModuleName, csharp_library,
                DllFileName, !IO),
            install_file(ProgressStream, Globals, DllFileName, GradeLibDir,
                LibsSucceeded, !IO),
            InitSucceeded = succeeded
        ;
            Target = target_java,
            linked_target_file_name(Globals, MainModuleName, java_archive,
                JarFileName, !IO),
            install_file(ProgressStream, Globals, JarFileName, GradeLibDir,
                LibsSucceeded, !IO),
            InitSucceeded = succeeded
        ;
            Target = target_c,
            linked_target_file_name(Globals, MainModuleName, static_library,
                LibFileName, !IO),
            linked_target_file_name(Globals, MainModuleName, shared_library,
                SharedLibFileName, !IO),
            maybe_install_library_file(ProgressStream, Globals, "static",
                LibFileName, GradeLibDir, LibSucceeded0, !IO),
            ( if LibFileName = SharedLibFileName then
                LibsSucceeded = LibSucceeded0
            else
                maybe_install_library_file(ProgressStream, Globals, "shared",
                    SharedLibFileName, GradeLibDir, SharedLibSucceeded, !IO),
                LibsSucceeded = LibSucceeded0 `and` SharedLibSucceeded
            ),
            install_grade_init(ProgressStream, Globals, GradeDir,
                MainModuleName, InitSucceeded, !IO)
        ),

        list.map_foldl2(
            install_grade_ints_and_headers(ProgressStream, Globals,
                LinkSucceeded, GradeDir),
            AllModuleNames, IntsHeadersSucceeded, !Info, !IO),
        Succeeded = and_list(
            [LibsSucceeded, InitSucceeded | IntsHeadersSucceeded])
    ;
        DirResult = did_not_succeed,
        Succeeded = did_not_succeed
    ).

    % Install the `.init' file for the current grade.
    %
:- pred install_grade_init(io.text_output_stream::in, globals::in,
    string::in, module_name::in, maybe_succeeded::out, io::di, io::uo) is det.

install_grade_init(ProgressStream, Globals, GradeDir, MainModuleName,
        Succeeded, !IO) :-
    % XXX Should we generalize get_std_grade_specific_install_lib_dir
    % to include this s/lib/modules/ version?
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    GradeModulesDir = Prefix / "lib" / "mercury" / "modules" / GradeDir,
    module_name_to_file_name(Globals, $pred, ext_cur_gs(ext_cur_gs_lib_init),
        MainModuleName, InitFileName),
    install_file(ProgressStream, Globals, InitFileName, GradeModulesDir,
        Succeeded, !IO).

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred install_grade_ints_and_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, string::in, module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_grade_ints_and_headers(ProgressStream, Globals, LinkSucceeded,
        GradeDir, ModuleName, Succeeded, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(_ModuleDepInfo),
        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix / "lib" / "mercury",

        globals.get_target(Globals, Target),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        ( if
            Target = target_c,
            HighLevelCode = yes
        then
            GradeIncDir = LibDir / "lib" / GradeDir / "inc",
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                GradeIncDir, ModuleName,
                {ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih), "mihs"},
                HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            IntDir = LibDir / "ints",
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                IntDir, ModuleName,
                {ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih), "mihs"},
                HeaderSucceeded2, !IO),
            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        else
            HeaderSucceeded = succeeded
        ),

        GradeIntDir = LibDir / "ints" / GradeDir,
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                GradeIntDir, ModuleName,
                {ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain),
                    "opts"},
                OptSucceeded, !IO)
        ;
            AnyIntermod = no,
            OptSucceeded = succeeded
        ),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                GradeIntDir, ModuleName,
                {ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
                    "analyses"},
                IntermodAnalysisSucceeded, !IO)
        ;
            IntermodAnalysis = no,
            IntermodAnalysisSucceeded = succeeded
        ),
        Succeeded = HeaderSucceeded `and` OptSucceeded `and`
            IntermodAnalysisSucceeded
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed
    ).

    % Install a file in the given directory, and in directory/Mercury/exts
    % if the symlinks for the subdirectories couldn't be created
    % (e.g. on Windows).
    %
:- pred install_subdir_file(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, dir_name::in, module_name::in, {ext, string}::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_subdir_file(ProgressStream, Globals, SubdirLinkSucceeded, InstallDir,
        ModuleName, {Ext, ExtDir}, Succeeded, !IO) :-
    module_name_to_file_name(Globals, $pred, Ext, ModuleName, FileName),
    install_file(ProgressStream, Globals, FileName, InstallDir,
        Succeeded1, !IO),
    (
        SubdirLinkSucceeded = did_not_succeed,
        install_file(ProgressStream, Globals, FileName,
            InstallDir / "Mercury" / ExtDir, Succeeded2, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ;
        SubdirLinkSucceeded = succeeded,
        Succeeded = Succeeded1
    ).

:- pred maybe_install_library_file(io.text_output_stream::in, globals::in,
    string::in, file_name::in, dir_name::in, maybe_succeeded::out,
    io::di, io::uo) is det.

maybe_install_library_file(ProgressStream, Globals, Linkage,
        FileName, InstallDir, Succeeded, !IO) :-
    globals.lookup_accumulating_option(Globals, lib_linkages, LibLinkages),
    ( if list.member(Linkage, LibLinkages) then
        install_file(ProgressStream, Globals, FileName, InstallDir,
            Succeeded0, !IO),

        % We need to update the archive index after we copy a .a file to
        % the installation directory, because the linkers on some OSs
        % complain if we don't.
        ( if
            Linkage = "static",
            Succeeded0 = succeeded
        then
            % Since mmc --make uses --use-subdirs, the above FileName will
            % be directory qualified. We don't care about the build
            % directory here so we strip that qualification off.
            BaseFileName = dir.det_basename(FileName),
            generate_archive_index(ProgressStream, Globals, BaseFileName,
                InstallDir, Succeeded, !IO)
        else
            Succeeded = Succeeded0
        )
    else
        Succeeded = succeeded
    ).

    % Generate (or update) the index for an archive file,
    % i.e. run ranlib on a .a file.
    %
:- pred generate_archive_index(io.text_output_stream::in, globals::in,
    file_name::in, dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.

generate_archive_index(ProgressStream, Globals, FileName, InstallDir,
        Succeeded, !IO) :-
    verbose_make_four_part_msg(Globals, "Generating archive index for file",
         FileName, "in", InstallDir, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    globals.lookup_string_option(Globals, ranlib_command, RanLibCommand),
    globals.lookup_string_option(Globals, ranlib_flags, RanLibFlags),
    % XXX What is the point of using more than one space?
    Command = string.join_list("    ", [
        quote_shell_cmd_arg(RanLibCommand),
        RanLibFlags,
        quote_shell_cmd_arg(InstallDir / FileName)
    ]),
    % XXX MAKE_STREAM
    CmdOutputStream = ProgressStream,
    invoke_system_command(Globals, ProgressStream,
        CmdOutputStream, cmd_verbose, Command, Succeeded, !IO).

%---------------------------------------------------------------------------%

:- pred install_file(io.text_output_stream::in, globals::in,
    file_name::in, dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.

install_file(ProgressStream, Globals, FileName, InstallDir, Succeeded, !IO) :-
    verbose_make_four_part_msg(Globals, "Installing file", FileName,
        "in", InstallDir, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    copy_file_to_directory(Globals, ProgressStream, FileName,
        InstallDir, Succeeded, !IO).

%---------------------------------------------------------------------------%

:- pred make_install_dirs(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, maybe_succeeded::out, io::di, io::uo) is det.

make_install_dirs(ProgressStream, Globals, DirSucceeded, LinkSucceeded, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix / "lib" / "mercury",
    make_directory(LibDir / "inc", DirSucceeded1, !IO),
    make_directory(LibDir / "modules", DirSucceeded2, !IO),

    IntsSubdir = LibDir / "ints" / "Mercury",
    make_directory(IntsSubdir, DirSucceeded3, !IO),
    DirSucceeded123 = [DirSucceeded1, DirSucceeded2, DirSucceeded3],

    Subdirs = ["int0s", "ints", "int2s", "int3s", "opts", "trans_opts",
        "mhs", "mihs", "module_deps"],

    globals.lookup_bool_option(Globals, use_symlinks, UseSymLinks),
    (
        UseSymLinks = yes,
        list.map_foldl(make_symlink_to_parent_dir(IntsSubdir), Subdirs,
            LinkSucceededSubdirs, !IO),
        LinkSucceeded = and_list(LinkSucceededSubdirs)
    ;
        UseSymLinks = no,
        LinkSucceeded = did_not_succeed
    ),
    (
        LinkSucceeded = succeeded,
        DirSucceededList = DirSucceeded123
    ;
        LinkSucceeded = did_not_succeed,
        % XXX This code does the right thing ONLY if either all elements
        % of LinkSucceededSubdirs are "succeeded", or if all elements
        % of LinkSucceededSubdirs are "did_not_succeeded".
        %
        % If some elements are "succeeded" and some are "did_not_succeeded",
        % then this code will attempt to make a directory with a name
        % that is already occupied by a symlink that was constructed just
        % above, fail with a misleading error message, and the final value
        % of DirSucceeded will be wrong.
        %
        % If we assume that the only possible reason for why making a symlink
        % would fail is the use_symlinks option being set to "no", then this
        % is fine. However, this is NOT the only possible reason.
        list.map_foldl(
            ( pred(ExtDir::in, ExtSucceeded::out, !.IO::di, !:IO::uo) is det:-
                make_directory(IntsSubdir / ExtDir, ExtSucceeded, !IO)
            ), Subdirs, ExtDirSucceededList, !IO),
        DirSucceededList = DirSucceeded123 ++ ExtDirSucceededList
    ),
    print_mkdir_errors(ProgressStream, DirSucceededList, DirSucceeded, !IO).

%---------------------%

:- pred make_grade_install_dirs(io.text_output_stream::in, globals::in,
    string::in, maybe_succeeded::out, maybe_succeeded::out,
    io::di, io::uo) is det.

make_grade_install_dirs(ProgressStream, Globals, Grade,
        DirSucceeded, LinkSucceeded, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix / "lib" / "mercury",

    GradeIntsSubdir = LibDir / "ints" / Grade / "Mercury",
    make_directory(GradeIntsSubdir, DirSucceeded1, !IO),

    GradeIncSubdir = LibDir / "lib" / Grade / "inc" / "Mercury",
    make_directory(GradeIncSubdir, DirSucceeded2, !IO),

    GradeModuleSubdir = LibDir / "modules" / Grade,
    make_directory(GradeModuleSubdir, DirSucceeded3, !IO),

    DirSucceeded123 = [DirSucceeded1, DirSucceeded2, DirSucceeded3],

    globals.lookup_bool_option(Globals, use_symlinks, UseSymLinks),
    (
        UseSymLinks = yes,
        make_symlink_to_parent_dir(GradeIncSubdir, "mihs",
            LinkSucceeded0, !IO),
        list.map_foldl(make_symlink_to_parent_dir(GradeIntsSubdir),
            ["opts", "trans_opts", "analyses"], LinkSucceededList, !IO),
        LinkSucceeded = and_list([LinkSucceeded0 | LinkSucceededList])
    ;
        UseSymLinks = no,
        LinkSucceeded = did_not_succeed
    ),
    (
        LinkSucceeded = succeeded,
        DirSucceededList = DirSucceeded123
    ;
        LinkSucceeded = did_not_succeed,
        % XXX The XXX in the corresponding position in make_install_dirs
        % above applies here as well.
        make_directory(GradeIncSubdir / "mihs", DirSucceeded4, !IO),
        make_directory(GradeIntsSubdir / "opts", DirSucceeded5, !IO),
        make_directory(GradeIntsSubdir / "trans_opts", DirSucceeded6, !IO),
        make_directory(GradeIntsSubdir / "analyses", DirSucceeded7, !IO),
        DirSucceededList = DirSucceeded123 ++
            [DirSucceeded4, DirSucceeded5, DirSucceeded6, DirSucceeded7]
    ),
    print_mkdir_errors(ProgressStream, DirSucceededList, DirSucceeded, !IO).

%---------------------%

    % XXX BAD_SYMLINK This upward-pointing symlink makes it impossible
    % back up a Mercury install directory using scp. This is because
    % scp treats symlinks not as symlinks, but as the file or directory
    % they point to, and copies that (in this case) directory.
    % That directory will of contain this same symlink, and scp gets
    % trapped, always copying the files in between in an infinite loop.
    %
:- pred make_symlink_to_parent_dir(string::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

make_symlink_to_parent_dir(Subdir, ExtDirName, Succeeded, !IO) :-
    LinkName = Subdir / ExtDirName,
    definitely_make_symlink("..", LinkName, Succeeded, !IO).

%---------------------%

:- pred print_mkdir_errors(io.text_output_stream::in, list(io.res)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

print_mkdir_errors(_ProgressStream, [], succeeded, !IO).
print_mkdir_errors(ProgressStream, [Result | Results], Succeeded, !IO) :-
    (
        Result = ok,
        print_mkdir_errors(ProgressStream, Results, Succeeded, !IO)
    ;
        Result = error(Error),
        ErrorMsg = io.error_message(Error),
        % XXX Error does not identify the directory. This should be fixed,
        % *if* we ever see this error message actually being triggered.
        io.format(ProgressStream,
            "Error creating installation directory: %s\n",
            [s(ErrorMsg)], !IO),
        print_mkdir_errors(ProgressStream, Results, _, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
:- end_module make.library_install.
%---------------------------------------------------------------------------%
