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

%---------------------------------------------------------------------------%

    % install_library(ProgressStream, Globals, MainModuleName, Succeeded,
    %   !Info, !IO)
    %
:- pred install_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.copy_util.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.timestamp.
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
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

install_library(ProgressStream, Globals, MainModuleName, !:Succeeded,
        !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
        DepsSucceeded, AllModuleNames0, !Info, !IO),
    AllModuleNames = set.to_sorted_list(AllModuleNames0),
    (
        DepsSucceeded = succeeded,
        install_library_non_grade_specific_files(ProgressStream, Globals,
            AllModuleNames, NgsLibDirMap, !:Succeeded, !Info, !IO),
        install_library_grade_specific_files_for_all_libgrades(ProgressStream,
            Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
            !Succeeded, !Info, !IO)
    ;
        DepsSucceeded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ).

:- pred install_library_non_grade_specific_files(io.text_output_stream::in,
    globals::in, list(module_name)::in, libdir_map::out, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_non_grade_specific_files(ProgressStream, Globals,
        AllModuleNames, NgsLibDirMap, !:Succeeded, !Info, !IO) :-
    make_install_dirs(ProgressStream, Globals,
        DirSucceeded, NgsLibDirMap, !IO),
    (
        DirSucceeded = succeeded,
        % Note that install_ints_and_headers_for_module actually installs
        % some grade-specific files in non-grade-specific directories.
        list.foldl3(
            legacy_install_ints_and_headers_for_module(ProgressStream, Globals,
                NgsLibDirMap),
            AllModuleNames, succeeded, !:Succeeded, !Info, !IO),

        legacy_install_extra_headers(ProgressStream, Globals, !Succeeded, !IO)
    ;
        DirSucceeded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ).

:- pred legacy_install_ints_and_headers_for_module(io.text_output_stream::in,
    globals::in, libdir_map::in, module_name::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

legacy_install_ints_and_headers_for_module(ProgressStream, Globals,
        NgsLibDirMap, ModuleName, !Succeeded, !Info, !IO) :-
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
            % There won't be any .int0 files to install.
            ExtExtDirs0 = []
        else
            ExtExtDirs0 = [{ext_cur_ngs(ext_cur_ngs_int_int0), "int0s"}]
        ),
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            ExtOpt = ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain),
            ExtExtDirs1 = [{ExtOpt, "opts"} | ExtExtDirs0]
        ;
            AnyIntermod = no,
            ExtExtDirs1 = ExtExtDirs0
        ),
        ExtExtDirs =
            [{ext_cur_ngs(ext_cur_ngs_int_int1), "ints"},
            {ext_cur_ngs(ext_cur_ngs_int_int2), "int2s"},
            {ext_cur_ngs(ext_cur_ngs_int_int3), "int3s"},
            {ext_cur_ngs(ext_cur_ngs_misc_module_dep), "module_deps"}
            | ExtExtDirs1],
        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix / "lib" / "mercury",
        list.foldl2(
            install_subdir_file(ProgressStream, Globals, NgsLibDirMap,
                LibDir / "ints", ModuleName),
            ExtExtDirs, !Succeeded, !IO),

        globals.get_target(Globals, Target),
        (
            Target = target_c,
            % `.mh' files are (were) only generated for modules containing
            % `:- pragma foreign_export' declarations.
            % But `.mh' files are expected by Mmake so always generate them,
            % otherwise there is trouble using libraries installed by
            % `mmc --make' with Mmake.
            % XXX If we ever phase out mmake we could revert this behaviour.
            % XXX Should we test
            % ModuleDepInfo ^ contains_foreign_export
            %   = contains_foreign_export?
            ExtMh = ext_cur_ngs_max_cur(ext_cur_ngs_max_cur_mh),
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred, ExtMh,
                ModuleName, FileName, _FileNameProposed),
            install_file(ProgressStream, Globals, FileName, LibDir / "inc",
                !Succeeded, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(ProgressStream, Globals, NgsLibDirMap,
                LibDir / "ints", ModuleName, {ExtMh, "mhs"}, !Succeeded, !IO)
        ;
            ( Target = target_java
            ; Target = target_csharp
            )
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        !:Succeeded = did_not_succeed
    ).

:- pred legacy_install_extra_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

legacy_install_extra_headers(ProgressStream, Globals, !Succeeded, !IO) :-
    globals.lookup_accumulating_option(Globals, extra_library_header,
        ExtraHdrs),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    IncDir = Prefix / "lib" / "mercury" / "inc",
    list.foldl2(install_file_to(ProgressStream, Globals, IncDir),
        ExtraHdrs, !Succeeded, !IO).

%---------------------------------------------------------------------------%

:- pred install_library_grade_specific_files_for_all_libgrades(
    io.text_output_stream::in, globals::in, libdir_map::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_grade_specific_files_for_all_libgrades(ProgressStream,
        Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO) :-
    grade_directory_component(Globals, CurGrade),
    % XXX With Mmake, LIBGRADES is target-specific; with this code in
    % mmc --make, it isn't.
    globals.lookup_accumulating_option(Globals, libgrades, LibGrades),
    NonCurLibGrades = list.delete_all(LibGrades, CurGrade),

    % The library is already built in the current grade; we just need to
    % install it. For all other grades, we must build the library first
    % in that grade before we can install it.
    legacy_install_library_grade_files(ProgressStream, Globals, NgsLibDirMap,
        CurGrade, MainModuleName, AllModuleNames, !Succeeded, !Info, !IO),
    KeepGoing = make_info_get_keep_going(!.Info),
    setup_make_and_install_library_grades(KeepGoing, ProgressStream,
        Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
        NonCurLibGrades, !Succeeded, !Info, !IO).

:- pred setup_make_and_install_library_grades(maybe_keep_going::in,
    io.text_output_stream::in, globals::in, libdir_map::in,
    module_name::in, list(module_name)::in, list(string)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

setup_make_and_install_library_grades(_, _, _, _, _, _, [],
        !Succeeded, !Info, !IO).
setup_make_and_install_library_grades(KeepGoing, ProgressStream, Globals,
        NgsLibDirMap, MainModuleName, AllModuleNames, [Grade | Grades],
        !Succeeded, !Info, !IO) :-
    should_we_stop_or_continue(KeepGoing, !.Succeeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        setup_make_and_install_library_grade(ProgressStream, Globals,
            NgsLibDirMap, MainModuleName, AllModuleNames, Grade,
            !Succeeded, !Info, !IO),
        setup_make_and_install_library_grades(KeepGoing, ProgressStream,
            Globals, NgsLibDirMap, MainModuleName, AllModuleNames, Grades,
            !Succeeded, !Info, !IO)
    ).

    % This predicate sets things up for
    %
    % - first making the library in the given grade,
    % - and then installing that library,
    %
    % and then invokes make_and_install_library_grade to actually do
    % both of those actions.
    %.
:- pred setup_make_and_install_library_grade( io.text_output_stream::in,
    globals::in, libdir_map::in, module_name::in, list(module_name)::in,
    string::in, maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

setup_make_and_install_library_grade(ProgressStream, Globals, NgsLibDirMap,
        MainModuleName, AllModuleNames, Grade, !Succeeded, !Info, !IO) :-
    % Only remove grade-dependent files after installing if
    % --use-grade-subdirs is not specified by the user.
    globals.get_subdir_setting(Globals, SubDirSetting),
    (
        ( SubDirSetting = use_cur_dir
        ; SubDirSetting = use_cur_ngs_subdir
        ),
        CleanAfter = yes
    ;
        SubDirSetting = use_cur_ngs_gs_subdir,
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
        !:Succeeded = did_not_succeed
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
        make_info_set_target_file_timestamp_map(init_target_file_timestamp_map,
            !Info),

        % We build the library in the new grade in a separate process
        % to make it easier to stop and clean up on an interrupt.
        globals.lookup_bool_option(LibGlobals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        call_in_forked_process(
            make_and_install_library_grade(ProgressStream, LibGlobals,
                NgsLibDirMap, MainModuleName, AllModuleNames,
                !.Info, CleanAfter),
            Succeeded0, !IO),
        CleanupPred = maybe_make_grade_clean(ProgressStream, LibGlobals,
            CleanAfter, MainModuleName, AllModuleNames),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
            Succeeded0, Succeeded1, !Info, !IO),
        !:Succeeded = !.Succeeded `and` Succeeded1
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

:- pred make_and_install_library_grade(io.text_output_stream::in,
    globals::in, libdir_map::in, module_name::in, list(module_name)::in,
    make_info::in, bool::in, maybe_succeeded::out, io::di, io::uo) is det.

make_and_install_library_grade(ProgressStream, Globals, NgsLibDirMap,
        MainModuleName, AllModuleNames, !.Info, CleanAfter, Succeeded, !IO) :-
    % This is the "make" part ...
    make_misc_target(ProgressStream, Globals,
        MainModuleName - misc_target_build_library, LibSucceeded,
        !Info, [], Specs, !IO),
    (
        LibSucceeded = succeeded,
        % ... and this is the "install" part.
        grade_directory_component(Globals, GradeDir),
        legacy_install_library_grade_files(ProgressStream, Globals,
            NgsLibDirMap, GradeDir, MainModuleName, AllModuleNames,
            succeeded, Succeeded, !Info, !IO),
        maybe_make_grade_clean(ProgressStream, Globals, CleanAfter,
            MainModuleName, AllModuleNames, !.Info, _Info, !IO)
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
    % XXX document the others ...
    %
:- pred legacy_install_library_grade_files(io.text_output_stream::in,
    globals::in, libdir_map::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

legacy_install_library_grade_files(ProgressStream, Globals, NgsLibDirMap,
        GradeDir, MainModuleName, AllModuleNames, !Succeeded, !Info, !IO) :-
    % This creates *some* of the directories into which we install
    % grade-specific files, but not all; some are created by the calls
    % to module_name_to_file_name_create_dirs below.
    make_grade_install_dirs(ProgressStream, Globals, GradeDir,
        DirSucceeded, GsLibDirMap, !IO),
    (
        DirSucceeded = succeeded,
        globals.get_target(Globals, Target),
        get_std_grade_specific_install_lib_dir(Globals, GradeDir, GradeLibDir),
        (
            Target = target_csharp,
            ExtDll = ext_cur_gas(ext_cur_gas_lib_dll),
            % XXX LEGACY
            module_name_to_file_name_create_dirs(Globals, $pred, ExtDll,
                MainModuleName, DllFileName, _DllFileNameProposed, !IO),
            install_file(ProgressStream, Globals, DllFileName, GradeLibDir,
                !Succeeded, !IO)
        ;
            Target = target_java,
            ExtJar = ext_cur_gs(ext_cur_gs_lib_jar),
            % XXX LEGACY
            module_name_to_file_name_create_dirs(Globals, $pred, ExtJar,
                MainModuleName, JarFileName, _JarFileNameProposed, !IO),
            install_file(ProgressStream, Globals, JarFileName, GradeLibDir,
                !Succeeded, !IO)
        ;
            Target = target_c,
            ExtA =  ext_cur_gas(ext_cur_gas_lib_lib_opt),
            ExtSo = ext_cur_gas(ext_cur_gas_lib_sh_lib_opt),
            % XXX LEGACY
            module_name_to_lib_file_name_create_dirs(Globals, $pred,
                "lib", ExtA, MainModuleName,
                StaticLibFileName, _StaticLibFileNameProposed, !IO),
            module_name_to_lib_file_name_create_dirs(Globals, $pred,
                "lib", ExtSo, MainModuleName,
                SharedLibFileName, _SharedLibFileNameProposed, !IO),
            maybe_install_static_or_dynamic_archive(ProgressStream,
                Globals, "static", StaticLibFileName, GradeLibDir,
                !Succeeded, !IO),
            ( if StaticLibFileName = SharedLibFileName then
                true
            else
                maybe_install_static_or_dynamic_archive(ProgressStream,
                    Globals, "shared", SharedLibFileName, GradeLibDir,
                    !Succeeded, !IO)
            ),
            install_grade_init(ProgressStream, Globals, GradeDir,
                MainModuleName, !Succeeded, !IO)
        ),

        list.foldl3(
            install_grade_ints_and_headers(ProgressStream, Globals,
                NgsLibDirMap, GsLibDirMap, GradeDir),
            AllModuleNames, !Succeeded, !Info, !IO)
    ;
        DirSucceeded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ).

    % Install the `.init' file for the current grade.
    %
:- pred install_grade_init(io.text_output_stream::in, globals::in,
    string::in, module_name::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

install_grade_init(ProgressStream, Globals, GradeDir, MainModuleName,
        !Succeeded, !IO) :-
    % XXX Should we generalize get_std_grade_specific_install_lib_dir
    % to include this s/lib/modules/ version?
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    GradeModulesDir = Prefix / "lib" / "mercury" / "modules" / GradeDir,
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred, ext_cur_gs(ext_cur_gs_lib_init),
        MainModuleName, InitFileName, _InitFileNameProposed),
    install_file(ProgressStream, Globals, InitFileName, GradeModulesDir,
        !Succeeded, !IO).

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred install_grade_ints_and_headers(io.text_output_stream::in, globals::in,
    libdir_map::in, libdir_map::in, string::in, module_name::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_grade_ints_and_headers(ProgressStream, Globals,
        NgsLibDirMap, GsLibDirMap, GradeDir, ModuleName,
        !Succeeded, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(_ModuleDepInfo),

        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix / "lib" / "mercury",

        % NOTE Before our ancestor install_library_grade_files gets invoked,
        % the grade-specific components of Globals, including Target and
        % HighLevelCode, will have been set up to reflect the grade
        % that we are installing.
        globals.get_target(Globals, Target),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        ( if
            Target = target_c,
            HighLevelCode = yes
        then
            GradeIncDir = LibDir / "lib" / GradeDir / "inc",
            ExtMih = ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih),
            install_subdir_file(ProgressStream, Globals, GsLibDirMap,
                GradeIncDir, ModuleName, {ExtMih, "mihs"}, !Succeeded, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            %
            % XXX BUG Why are we installing to a NON-GRADE-SPECIFIC directory
            % in a predicate that does installs of GRADE-SPECIFIC files?
            % Any installs done by this code for one grade will be overwritten
            % by the install done by the next grade.
            IntsDir = LibDir / "ints",
            install_subdir_file(ProgressStream, Globals, NgsLibDirMap,
                IntsDir, ModuleName, {ExtMih, "mihs"}, !Succeeded, !IO)
        else
            true
        ),

        GradeIntsDir = LibDir / "ints" / GradeDir,
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            ExtOpt = ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain),
            install_subdir_file(ProgressStream, Globals, GsLibDirMap,
                GradeIntsDir, ModuleName, {ExtOpt, "opts"}, !Succeeded, !IO)
        ;
            AnyIntermod = no
        ),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            ExtAn = ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
            install_subdir_file(ProgressStream, Globals, GsLibDirMap,
                GradeIntsDir, ModuleName, {ExtAn, "analyses"}, !Succeeded, !IO)
        ;
            IntermodAnalysis = no
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

    % Install a file in the given directory, and in directory/Mercury/exts
    % if the symlinks for the subdirectories couldn't be created
    % (e.g. on Windows).
    %
    % XXX Rename and redocument.
    %
    % TODO: have our callers compute ExtDir from Ext
    %
    % TODO: delete the InstallDir argument after a week or two of usage
    % *without* an assertion failure.
    %
:- pred install_subdir_file(io.text_output_stream::in, globals::in,
    libdir_map::in, dir_name::in, module_name::in, {ext, string}::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

install_subdir_file(ProgressStream, Globals, LibDirMap, InstallDir,
        ModuleName, {Ext, ExtDir}, !Succeeded, !IO) :-
    % NOTE The calls to install_file will use any directory name components
    % of FileName to *find* the file to install, but the name of the
    % installed file will include *only* the base name of FileName.
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred, Ext, ModuleName,
        FileName, _FileNameProposed),
    map.lookup(LibDirMap, ExtDir, InstallTo),
    (
        InstallTo = install_to_cur_only(CurDir),
        expect(unify(InstallDir, CurDir), $pred, "InstallDir != CurDir"),
        install_file(ProgressStream, Globals, FileName, CurDir,
            !Succeeded, !IO)
    ;
        InstallTo = install_to_cur_ngs(CurDir, NgsDir),
        expect(unify(InstallDir, CurDir), $pred, "InstallDir != CurDir"),
        install_file(ProgressStream, Globals, FileName, CurDir,
            !Succeeded, !IO),
        expect(unify(InstallDir / "Mercury" / ExtDir, NgsDir), $pred,
            "InstallDir != NgsDir"),
        install_file(ProgressStream, Globals, FileName, NgsDir,
            !Succeeded, !IO)
    ).

%---------------------%

:- pred maybe_install_static_or_dynamic_archive(io.text_output_stream::in,
    globals::in, string::in, file_name::in, dir_name::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

maybe_install_static_or_dynamic_archive(ProgressStream, Globals, Linkage,
        FileName, InstallDir, !Succeeded, !IO) :-
    globals.lookup_accumulating_option(Globals, lib_linkages, LibLinkages),
    ( if list.member(Linkage, LibLinkages) then
        install_file(ProgressStream, Globals, FileName, InstallDir,
            succeeded, InstallSucceeded0, !IO),

        % We need to update the archive index after we copy it to the
        % installation directory, because the linkers on some OSs complain
        % if we don't.
        ( if
            Linkage = "static",
            InstallSucceeded0 = succeeded
        then
            BaseFileName = dir.det_basename(FileName),
            InstalledFileName = InstallDir / BaseFileName,
            generate_archive_index(ProgressStream, Globals, InstalledFileName,
                RanlibSucceeded, !IO),
            !:Succeeded = !.Succeeded `and` RanlibSucceeded
        else
            !:Succeeded = !.Succeeded `and` InstallSucceeded0
        )
    else
        true
    ).

    % Generate (or update) the index for an archive file,
    % i.e. run ranlib on a .a file.
    %
:- pred generate_archive_index(io.text_output_stream::in, globals::in,
    file_name::in, maybe_succeeded::out, io::di, io::uo) is det.

generate_archive_index(ProgressStream, Globals, FileName, Succeeded, !IO) :-
    verbose_make_two_part_msg(Globals, "Generating archive index for",
         FileName, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    globals.lookup_string_option(Globals, ranlib_command, RanLibCommand),
    globals.lookup_string_option(Globals, ranlib_flags, RanLibFlags),
    % XXX Why are we executing Command if RanLibCommand is the empty string?
    % juliensf says: "Technically a bug, but with the current configuration
    % script and set of OSs we support, it shouldn't ever happen except by
    % someone manually editing a configuration file. (That's likely to break
    % their Mercury installation ...)"
    Command = string.join_list("    ", [
        % Note that it is possible, though not likely, that the ranlib_command
        % option might be set to a command name that requires quoting. juliensf
        % says: "The C compiler toolchains and binutils let users specify
        % suffixes that are included in the executable names -- this is
        % usually use in cross compiler toolchains."
        quote_shell_cmd_arg(RanLibCommand),
        RanLibFlags,
        quote_shell_cmd_arg(FileName)
    ]),
    % XXX MAKE_STREAM
    CmdOutputStream = ProgressStream,
    invoke_system_command(Globals, ProgressStream,
        CmdOutputStream, cmd_verbose, Command, Succeeded, !IO).

%---------------------%

:- pred install_file_to(io.text_output_stream::in, globals::in,
    dir_name::in, file_name::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

install_file_to(ProgressStream, Globals, InstallDir, FileName,
        !Succeeded, !IO) :-
    install_file(ProgressStream, Globals, FileName, InstallDir,
        !Succeeded, !IO).

:- pred install_file(io.text_output_stream::in, globals::in,
    file_name::in, dir_name::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

install_file(ProgressStream, Globals, FileName, InstallDir, !Succeeded, !IO) :-
    verbose_make_four_part_msg(Globals, "Installing file", FileName,
        "in", InstallDir, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    copy_file_to_directory(Globals, ProgressStream, FileName,
        InstallDir, CopySucceeded, !IO),
    !:Succeeded = !.Succeeded `and` CopySucceeded.

%---------------------------------------------------------------------------%

:- pred make_install_dirs(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, libdir_map::out, io::di, io::uo) is det.

make_install_dirs(ProgressStream, Globals, !:DirSucceeded,
        !:NgsLibDirMap, !IO) :-
    !:DirSucceeded = succeeded,
    map.init(!:NgsLibDirMap),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix / "lib" / "mercury",
    make_nonext_dir(ProgressStream, LibDir / "inc", !DirSucceeded, !IO),
    make_nonext_dir(ProgressStream, LibDir / "modules", !DirSucceeded, !IO),

    IntsSubDir = LibDir / "ints",
    make_nonext_dir(ProgressStream, IntsSubDir / "Mercury",
        !DirSucceeded, !IO),

    SubDirs = ["int0s", "ints", "int2s", "int3s", "opts",
        "mhs", "mihs", "module_deps"],

    globals.lookup_bool_option(Globals, use_symlinks, UseSymLinks),
    (
        UseSymLinks = yes,
        % NOTE The point of using symlinks here is to save some space
        % in the install directory. We want install non-grade-specific files
        % into e.g. *both*
        %
        %   LibDir / "ints"
        %       for compiler invocations with --no-use-subdirs
        %
        % *and* into
        %
        %   LibDir / "ints" / "Mercury" / ExtDir
        %       for compiler invocations with --use-subdirs
        %
        % where ExtDir is the extension-specific directory name, such as
        % "int0s".
        %
        % By making the latter pathname a symlink to the former, a single
        % copy will be found by both kinds of compiler invocations.
        %
        % XXX Another way to accomplish the same goal would be to copy
        % e.g. .int0 files to LibDir / "ints" / "Mercury" / "int0s", and
        % add a symlink to that file to LibDir / "ints". The main benefit of
        % that approach would be the avoidance of the upward-pointing symlink,
        % which makes it impossibe to back up install directories using scp.
        % A minor benefit is the avoidance of the need to traverse a symlink
        % during --use-subdirs compiler invocations, with a corresponding new
        % minor cost being the introduction of the need to traverse a symlink
        % during --no-use-subdirs compiler invocations.
        list.foldl3(
            make_ngs_dir_symlink_to_cur(ProgressStream, IntsSubDir),
            SubDirs, !DirSucceeded, !NgsLibDirMap, !IO)
    ;
        UseSymLinks = no,
        list.foldl3(
            make_ngs_dir(ProgressStream, IntsSubDir),
            SubDirs, !DirSucceeded, !NgsLibDirMap, !IO)
    ).

%---------------------%

:- pred make_grade_install_dirs(io.text_output_stream::in, globals::in,
    string::in, maybe_succeeded::out, libdir_map::out,
    io::di, io::uo) is det.

make_grade_install_dirs(ProgressStream, Globals, GradeDir,
        !:DirSucceeded, !:GsLibDirMap, !IO) :-
    !:DirSucceeded = succeeded,
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix / "lib" / "mercury",

    GradeIncSubDir = LibDir / "lib" / GradeDir / "inc",
    GradeIntsSubDir = LibDir / "ints" / GradeDir,
    GradeModuleSubDir = LibDir / "modules" / GradeDir,

    make_nonext_dir(ProgressStream, GradeIncSubDir / "Mercury",
        !DirSucceeded, !IO),
    make_nonext_dir(ProgressStream, GradeIntsSubDir / "Mercury",
        !DirSucceeded, !IO),
    make_nonext_dir(ProgressStream, GradeModuleSubDir, !DirSucceeded, !IO),

    map.init(!:GsLibDirMap),

    globals.lookup_bool_option(Globals, use_symlinks, UseSymLinks),
    (
        UseSymLinks = yes,
        % XXX This code seems strange, because code using mmc --make
        % with --use-grade-subdirs should look for grade-specific files *only*
        % in LibDir / "ints" / GradeDir / "Mercury" / ExtDir, and *never*
        % in LibDir / "ints" / GradeDir.
        %
        % I (zs) can think of two possible reasons for using symlinks here.
        % One is that the original author of this code for creating the
        % directories for grade-specific files reused the code for
        % non-grade-specific files, even though it was not designed for this
        % purpose. The other is that this reuse also made the grade-specific
        % install directories sort-of isomorphic to the non-grade-specific
        % install directories, which allows code NOT using either mmc --make
        % or --use-grade-subdirs to use the same VPATH mechanism to look
        % inside both, just by specifying the appropriate starting path name
        % for each. (The "sort-of" is there because the non-grade-specific
        % install directories *contain* the grade-specific ones.)
        make_ngs_dir_symlink_to_cur(ProgressStream, GradeIncSubDir, "mihs",
            !DirSucceeded, !GsLibDirMap, !IO),
        list.foldl3(
            make_ngs_dir_symlink_to_cur(ProgressStream, GradeIntsSubDir),
            ["opts", "analyses"],
            !DirSucceeded, !GsLibDirMap, !IO)
    ;
        UseSymLinks = no,
        make_ngs_dir(ProgressStream, GradeIncSubDir, "mihs",
            !DirSucceeded, !GsLibDirMap, !IO),
        make_ngs_dir(ProgressStream, GradeIntsSubDir, "opts",
            !DirSucceeded, !GsLibDirMap, !IO),
        make_ngs_dir(ProgressStream, GradeIntsSubDir, "analyses",
            !DirSucceeded, !GsLibDirMap, !IO)
    ).

%---------------------%

    % Map from the directory name associated with a given extension
    % (such as "int0s" for .int0 files, or "analyses" for .analysis files)
    % to the pathnames of the one or two directories we have created to store
    % files with that extension.
    %
    % We use libdir_maps for extensions whose files are installed using
    % install_subdir_file. Extensions whose files are installed directly
    % with install_file will not appear in maps of this type.
    %
:- type libdir_map == map(string, libdir_info).

:- type libdir_info
    --->    install_to_cur_ngs(dir_name, dir_name)
            % Install files to both the cur directory (the first argument) and
            % the non-grade-specific or ngs directory (the second argument).
            %
            % For non-grade-specific extensions, the first directory is
            % the one where the installed file is intended to be found by
            % --no-use-subdirs compiler invocations, while the second is for
            % --use-subdirs compiler invocations.
            %
            % For grade-specific extensions, we use the same setup.
            % I (zs) am not sure why, but my guess is documented in the
            % big comment in make_grade_install_dirs.
    ;       install_to_cur_only(dir_name).
            % Install files only to the specified directory.
            % This will be the cur directory, and the ngs directory
            % will be a symlink to the cur directory.

%---------------------%

:- pred make_nonext_dir(io.text_output_stream::in, dir_name::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

make_nonext_dir(ProgressStream, DirName, !Succeeded, !IO) :-
    make_directory(DirName, IOResult, !IO),
    print_any_error(ProgressStream, DirName, IOResult, !Succeeded, !IO).

:- pred make_ngs_dir(io.text_output_stream::in, dir_name::in, file_name::in,
    maybe_succeeded::in, maybe_succeeded::out,
    libdir_map::in, libdir_map::out, io::di, io::uo) is det.

make_ngs_dir(ProgressStream, CurDir, ExtDirName,
        !Succeeded, !LibDirMap, !IO) :-
    NgsDir = CurDir / "Mercury" / ExtDirName,
    make_directory(NgsDir, IOResult, !IO),
    print_any_error(ProgressStream, NgsDir, IOResult, !Succeeded, !IO),
    map.det_insert(ExtDirName, install_to_cur_ngs(CurDir, NgsDir), !LibDirMap).

    % XXX BAD_SYMLINK This upward-pointing symlink makes it impossible
    % to back up a Mercury install directory using scp. This is because
    % scp treats symlinks not as symlinks, but as the file or directory
    % they point to, and copies that (in this case) directory.
    % That directory will of course contain this same symlink, and scp gets
    % trapped, always copying the files in between in an infinite loop,
    % which ends only when it has completely filled up the target filesystem.
    %
    % Another minor problem is that "diff -R" will refuse to traverse
    % symlinks to ".." when comparing e.g. two install directories.
    %
:- pred make_ngs_dir_symlink_to_cur(io.text_output_stream::in,
    dir_name::in, file_name::in, maybe_succeeded::in, maybe_succeeded::out,
    libdir_map::in, libdir_map::out, io::di, io::uo) is det.

make_ngs_dir_symlink_to_cur(ProgressStream, CurDir, ExtDirName,
        !Succeeded, !LibDirMap, !IO) :-
    NgsDir = CurDir / "Mercury" / ExtDirName,
    definitely_make_symlink("..", NgsDir, LinkSucceeded, !IO),
    (
        LinkSucceeded = succeeded,
        % If CurDir / "Mercury" / ExtDirName is a symlink to "..", then
        % it points to CurDir.
        LibDirInfo = install_to_cur_only(CurDir),
        map.det_insert(ExtDirName, LibDirInfo, !LibDirMap)
    ;
        LinkSucceeded = did_not_succeed,
        % We don't print an error message if making the *symlink* fails;
        % we only print one if making a *directory* fails.
        make_ngs_dir(ProgressStream, CurDir, ExtDirName,
            !Succeeded, !LibDirMap, !IO)
    ).

%---------------------%

:- pred print_any_error(io.text_output_stream::in, dir_name::in, io.res::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

print_any_error(ProgressStream, DirName, Result, !Succeeded, !IO) :-
    (
        Result = ok
    ;
        Result = error(Error),
        ErrorMsg = io.error_message(Error),
        io.format(ProgressStream,
            "Error creating installation directory %s: %s\n",
            [s(DirName), s(ErrorMsg)], !IO),
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
:- end_module make.library_install.
%---------------------------------------------------------------------------%
