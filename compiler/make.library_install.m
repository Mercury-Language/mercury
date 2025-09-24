%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
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
    %   !Info, !IO):
    %
    % Install
    %
    % - all non-grade-specific (and if applicable, pseudo-grade-specific) files
    %   for the current grade, all of which we presume have been built already,
    %   and
    %
    % - all grade-specific files for all library grades, which we presume
    %   to have been built already *only* for the current grade.
    %
    % The installs are *always* done to the destinations required by the
    % LEGACY install directory structure, and are *also* done to the PROPOSED
    % install directory structure *if* the --experiment4 option is given.
    %
:- pred install_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % install_library_gs_gas(ProgressStream, Globals, MainModuleName,
    %   Succeeded, !Info, !IO):
    %
    % Install
    %
    % - all grade-specific files for the current grade, which we presume
    %   to have been built already.
    %
    % This target is designed to be used by the code that does PROPOSED
    % library installs for mmake. When that code installs a library,
    % it needs to be able to install the grade-specific files of *all*
    % libgrades, including the ones that mmake does not support (the Java
    % and C# grades). The relevant mmake rules are constructed by code in
    % generate_mmakefile_fragments.m, and they are then put into the .dep file
    % of the main module of the library,
    %
    % Installs are *always* done to the destinations required by the
    % PROPOSED install directory structure. This is because the mmake target
    % that this predicate implements is used *only* by code that assumes
    % that structure.
    %
:- pred install_library_gs_gas(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

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
:- import_module cord.
:- import_module dir.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

install_library(ProgressStream, Globals, MainModuleName, !:Succeeded,
        !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
        DepsSucceeded, AllModuleNamesSet, !Info, !IO),
    set.to_sorted_list(AllModuleNamesSet, AllModuleNames),
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

install_library_gs_gas(ProgressStream, Globals, MainModuleName, !:Succeeded,
        !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
        DepsSucceeded, AllModuleNamesSet, !Info, !IO),
    set.to_sorted_list(AllModuleNamesSet, AllModuleNames),
    (
        DepsSucceeded = succeeded,
        globals.get_grade_dir(Globals, CurGrade),
        proposed_install_library_grade_specific_files_for_grade(ProgressStream,
            Globals, CurGrade, MainModuleName, AllModuleNames,
            succeeded, !:Succeeded, !Info, !IO)
    ;
        DepsSucceeded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% The code to install non-grade-specific files for both the LEGACY
% and the (eventually) PROPOSED install directory structure.
%

:- pred install_library_non_grade_specific_files(io.text_output_stream::in,
    globals::in, list(module_name)::in, libdir_map::out, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_non_grade_specific_files(ProgressStream, Globals,
        AllModuleNames, NgsLibDirMap, !:Succeeded, !Info, !IO) :-
    legacy_install_library_non_grade_specific_files(ProgressStream, Globals,
        AllModuleNames, NgsLibDirMap, LegacySucceeded, !Info, !IO),
    globals.lookup_bool_option(Globals, experiment4, InstallProposed),
    (
        InstallProposed = no,
        !:Succeeded = LegacySucceeded
    ;
        InstallProposed = yes,
        proposed_install_library_non_grade_specific_files(ProgressStream,
            Globals, AllModuleNames, ProposedSucceeded, !Info, !IO),
        !:Succeeded = LegacySucceeded `and` ProposedSucceeded
    ).

%---------------------------------------------------------------------------%
%
% The code to install non-grade-specific files for the LEGACY install
% directory structure.
%

:- pred legacy_install_library_non_grade_specific_files(
    io.text_output_stream::in, globals::in,
    list(module_name)::in, libdir_map::out, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

legacy_install_library_non_grade_specific_files(ProgressStream, Globals,
        AllModuleNames, NgsLibDirMap, !:Succeeded, !Info, !IO) :-
    legacy_make_non_grade_specific_install_dirs(ProgressStream, Globals,
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

%---------------------%

:- pred legacy_make_non_grade_specific_install_dirs(io.text_output_stream::in,
    globals::in, maybe_succeeded::out, libdir_map::out, io::di, io::uo) is det.

legacy_make_non_grade_specific_install_dirs(ProgressStream, Globals,
        !:DirSucceeded, !:NgsLibDirMap, !IO) :-
    !:DirSucceeded = succeeded,
    map.init(!:NgsLibDirMap),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix / "lib" / "mercury",
    legacy_make_nonext_dir(ProgressStream, LibDir / "inc",
        !DirSucceeded, !IO),
    legacy_make_nonext_dir(ProgressStream, LibDir / "modules",
        !DirSucceeded, !IO),

    IntsSubDir = LibDir / "ints",
    legacy_make_nonext_dir(ProgressStream, IntsSubDir / "Mercury",
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
            legacy_make_ngs_dir_symlink_to_cur(ProgressStream, IntsSubDir),
            SubDirs, !DirSucceeded, !NgsLibDirMap, !IO)
    ;
        UseSymLinks = no,
        list.foldl3(
            legacy_make_ngs_dir(ProgressStream, IntsSubDir),
            SubDirs, !DirSucceeded, !NgsLibDirMap, !IO)
    ).

%---------------------%

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
            % XXX LEGACY
            ExtOpt = ext_cur_ngs_gs_max_ngs(
                ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
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
            legacy_install_subdir_file(ProgressStream, Globals, NgsLibDirMap,
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
            ExtMh = ext_cur_pgs_max_cur(ext_cur_pgs_max_cur_mh),
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred, ExtMh,
                ModuleName, FileName, _FileNameProposed),
            install_file(ProgressStream, Globals, FileName, LibDir / "inc",
                !Succeeded, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            legacy_install_subdir_file(ProgressStream, Globals, NgsLibDirMap,
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

%---------------------%

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
%
% The code to install non-grade-specific files for the PROPOSED install
% directory structure.
%

:- pred proposed_install_library_non_grade_specific_files(
    io.text_output_stream::in, globals::in,
    list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

proposed_install_library_non_grade_specific_files(ProgressStream, Globals,
        AllModuleNames, !:Succeeded, !Info, !IO) :-
    gather_module_dep_infos(ProgressStream, Globals, AllModuleNames,
        ModulesWithChildren, DepInfoSucceded, !Info, !IO),
    (
        DepInfoSucceded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ;
        DepInfoSucceded = succeeded,
        !:Succeeded = succeeded,

        globals.lookup_string_option(Globals, install_prefix, Prefix0),
        Prefix = Prefix0 / "MercurySystem",

        make_dir_handle_any_error(ProgressStream, Prefix,
            MakePrefixDirSucceeded, !IO),
        (
            MakePrefixDirSucceeded = did_not_succeed
        ;
            MakePrefixDirSucceeded = succeeded,
            ExtInt0 = ext_cur_ngs(ext_cur_ngs_int_int0) : ext_cur_ngs_ns,
            ExtInt1 = ext_cur_ngs(ext_cur_ngs_int_int1) : ext_cur_ngs_ns,
            ExtInt2 = ext_cur_ngs(ext_cur_ngs_int_int2) : ext_cur_ngs_ns,
            ExtInt3 = ext_cur_ngs(ext_cur_ngs_int_int3) : ext_cur_ngs_ns,
            proposed_install_all_ngs_files(ProgressStream, Globals, Prefix,
                ExtInt0, ModulesWithChildren, !Succeeded, !IO),
            proposed_install_all_ngs_files(ProgressStream, Globals, Prefix,
                ExtInt1, AllModuleNames, !Succeeded, !IO),
            proposed_install_all_ngs_files(ProgressStream, Globals, Prefix,
                ExtInt2, AllModuleNames, !Succeeded, !IO),
            proposed_install_all_ngs_files(ProgressStream, Globals, Prefix,
                ExtInt3, AllModuleNames, !Succeeded, !IO),

            % XXX There is a potential problem here. We install .mh files,
            % which are not-grade-specific beyond being C-specific, if
            % the *current* grade targets C. However, if
            %
            % - the current grade targets a language *other than C*, but
            % - some other libgrade *does target C*,
            %
            % then no .mh file will get installed.
            %
            % We could avoid this by making .mh files grade-specific
            % (which, in a way, they are), but that would be inconvenient
            % for users, since #include statements for these .mh files
            % in their handwritten C code would have to be steered *somehow*
            % to the directory containing that .mh file *some* installed
            % C grade. That may, or may not, be the current grade, but
            % having to keep track of *two* grades, not one, in the build
            % infrastructure would be annoying.
            %
            % The right solution is probably
            %
            % - to wrap a "do this only if not already done" wrapper around
            %   this code,
            % - to put that wrapped code in a separate predicate,
            % - and invoke that predicate both here and in the code that
            %   installs the grade-specific files.
            globals.get_target(Globals, Target),
            (
                Target = target_c,
                % Once upon a time, we generated `.mh' files only for modules
                % containing `:- pragma foreign_export' declarations.
                % (See ModuleDepInfo ^ contains_foreign_export.)
                % But `.mh' files are expected by Mmake, so now we always
                % generate them. If we didn't, mmake would have trouble
                % when using libraries installed by `mmc --make'.
                ExtMh = ext_cur_pgs_max_cur(ext_cur_pgs_max_cur_mh),
                proposed_install_all_ngs_files(ProgressStream, Globals, Prefix,
                    ExtMh, AllModuleNames, !Succeeded, !IO),

                proposed_install_extra_headers(ProgressStream, Globals, Prefix,
                    !Succeeded, !IO)
            ;
                ( Target = target_java
                ; Target = target_csharp
                )
            )
        )
    ).

%---------------------%

:- type ext_cur_ngs_ns =< ext
    --->    ext_cur_ngs(ext_cur_ngs)
    ;       ext_cur_pgs_max_cur(ext_cur_pgs_max_cur).

:- pred proposed_install_all_ngs_files(io.text_output_stream::in, globals::in,
    string::in, ext_cur_ngs_ns::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

proposed_install_all_ngs_files(ProgressStream, Globals, Prefix,
        Ext, ModuleNames, !Succeeded, !IO) :-
    (
        Ext = ext_cur_ngs(ExtNgs),
        ext_cur_ngs_extension_dir(ExtNgs, _, ExtDirName)
    ;
        Ext = ext_cur_pgs_max_cur(ExtPgsMaxCur),
        ext_cur_pgs_max_cur_extension_dir(ExtPgsMaxCur, _, ExtDirName)
    ),

    InstallDir = Prefix / ExtDirName,
    make_dir_handle_any_error(ProgressStream, InstallDir,
        MakeInstallDirSucceeded, !IO),
    (
        MakeInstallDirSucceeded = did_not_succeed
    ;
        MakeInstallDirSucceeded = succeeded,
        GenExt = coerce(Ext),
        list.map(module_name_to_workspace_file_name(Globals, GenExt),
            ModuleNames, FileNames),
        install_files_to(ProgressStream, Globals, InstallDir,
            FileNames, !Succeeded, !IO)
    ).

:- pred proposed_install_extra_headers(io.text_output_stream::in, globals::in,
    string::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

proposed_install_extra_headers(ProgressStream, Globals, Prefix,
        !Succeeded, !IO) :-
    globals.lookup_accumulating_option(Globals, extra_library_header,
        ExtraHdrFileNames),
    % We could install extra headers to a specialized directory,
    % or we could install them to the directory to which we install
    % either .mh or .mih files. Since .mih files are internal details
    % of the Mercury implementation, that choice looks wrong. The other two
    % are definitely defensible. The code below chooses the second alternative,
    % installing to the directory containing .mh files. The main advantage
    % of this choice is that it avoids the extra complication that an extra
    % directory to search for would mean for invoking the C compiler.
    ExtPgsMaxCur = ext_cur_pgs_max_cur_mh,
    ext_cur_pgs_max_cur_extension_dir(ExtPgsMaxCur, _, ExtDirName),

    InstallDir = Prefix / ExtDirName,
    make_dir_handle_any_error(ProgressStream, InstallDir,
        MakeInstallDirSucceeded, !IO),
    (
        MakeInstallDirSucceeded = did_not_succeed
    ;
        MakeInstallDirSucceeded = succeeded,
        install_files_to(ProgressStream, Globals, InstallDir,
            ExtraHdrFileNames, !Succeeded, !IO)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The code to install grade-specific files for both the LEGACY
% and the (eventually) PROPOSED install directory structure.
%

:- pred install_library_grade_specific_files_for_all_libgrades(
    io.text_output_stream::in, globals::in, libdir_map::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_grade_specific_files_for_all_libgrades(ProgressStream,
        Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO) :-
    % The library is already built in the current grade; we just need to
    % install it. For all other grades, we must build the library first
    % in that grade before we can install it.
    globals.get_grade_dir(Globals, CurGrade),
    install_library_grade_specific_files_for_grade(ProgressStream, Globals,
        NgsLibDirMap, CurGrade, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO),

    % XXX With Mmake, LIBGRADES is target-specific; with this code in
    % mmc --make, it isn't.
    globals.lookup_accumulating_option(Globals, library_install_grades,
        LibGrades),
    NonCurLibGrades = list.delete_all(LibGrades, CurGrade),
    KeepGoing = make_info_get_keep_going(!.Info),
    setup_make_and_install_grade_specific_files_for_grades(ProgressStream,
        KeepGoing, Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
        NonCurLibGrades, !Succeeded, !Info, !IO).

:- pred setup_make_and_install_grade_specific_files_for_grades(
    io.text_output_stream::in, maybe_keep_going::in, globals::in,
    libdir_map::in, module_name::in, list(module_name)::in, list(string)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

setup_make_and_install_grade_specific_files_for_grades(_, _, _, _, _, _, [],
        !Succeeded, !Info, !IO).
setup_make_and_install_grade_specific_files_for_grades(ProgressStream,
        KeepGoing, Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
        [Grade | Grades], !Succeeded, !Info, !IO) :-
    should_we_stop_or_continue(KeepGoing, !.Succeeded, StopOrContinue,
        !Succeeded),
    (
        StopOrContinue = soc_stop
    ;
        StopOrContinue = soc_continue,
        setup_make_and_install_grade_specific_files_for_grade(ProgressStream,
            Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
            Grade, !Succeeded, !Info, !IO),
        setup_make_and_install_grade_specific_files_for_grades(ProgressStream,
            KeepGoing, Globals, NgsLibDirMap, MainModuleName, AllModuleNames,
            Grades, !Succeeded, !Info, !IO)
    ).

    % This predicate sets things up for
    %
    % - first making the library in the given grade,
    % - and then installing that library,
    %
    % and then invokes make_and_install_grade_specific_files_for_grades
    % to actually do both of those actions.
    %.
:- pred setup_make_and_install_grade_specific_files_for_grade(
    io.text_output_stream::in, globals::in, libdir_map::in,
    module_name::in, list(module_name)::in, string::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

setup_make_and_install_grade_specific_files_for_grade(ProgressStream, Globals,
        NgsLibDirMap, MainModuleName, AllModuleNames, Grade,
        !Succeeded, !Info, !IO) :-
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

    EnvOptFileVariables = make_info_get_env_optfile_variables(!.Info),
    lookup_mmc_options(EnvOptFileVariables, MaybeMCFlags),
    (
        MaybeMCFlags = ok1(MCFlags),
        get_default_options(Globals, DefaultOptionTable),
        MaybeStdLibGrades = make_info_get_maybe_stdlib_grades(!.Info),
        AllFlags = MCFlags ++ EnvVarArgs ++ OptionArgs,
        lookup_mercury_stdlib_dir(EnvOptFileVariables,
            MaybeEnvOptFileStdLibDirs),
        handle_given_options(ProgressStream, DefaultOptionTable,
            MaybeStdLibGrades, MaybeEnvOptFileStdLibDirs, AllFlags, _, _,
            OptionsSpecs, LibGlobals, !IO)
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
        %   StatusMap = version_hash_table.init_default(target_id_hash)
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
        StatusMap0 = make_info_get_target_status_map(!.Info),
        version_hash_table.fold(remove_target_file_if_grade_dependent,
            StatusMap0, StatusMap0, StatusMap),

        make_info_set_target_status_map(StatusMap, !Info),
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
            make_and_install_grade_specific_files_for_grades(ProgressStream,
                LibGlobals, NgsLibDirMap, MainModuleName, AllModuleNames,
                !.Info, CleanAfter),
            Succeeded0, !IO),
        CleanupPred = maybe_make_grade_clean(ProgressStream, LibGlobals,
            CleanAfter, MainModuleName, AllModuleNames),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
            Succeeded0, Succeeded1, !Info, !IO),
        !:Succeeded = !.Succeeded `and` Succeeded1
    ).

:- pred make_and_install_grade_specific_files_for_grades(
    io.text_output_stream::in, globals::in, libdir_map::in,
    module_name::in, list(module_name)::in, make_info::in, bool::in,
    maybe_succeeded::out, io::di, io::uo) is det.

make_and_install_grade_specific_files_for_grades(ProgressStream, Globals,
        NgsLibDirMap, MainModuleName, AllModuleNames, !.Info, CleanAfter,
        Succeeded, !IO) :-
    % This is the "make" part ...
    make_misc_target(ProgressStream, Globals,
        MainModuleName - misc_target_build_library, LibSucceeded,
        !Info, [], Specs, !IO),
    (
        LibSucceeded = succeeded,
        % ... and this is the "install" part.
        globals.get_grade_dir(Globals, GradeDir),
        install_library_grade_specific_files_for_grade(ProgressStream, Globals,
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
:- pred install_library_grade_specific_files_for_grade(
    io.text_output_stream::in, globals::in, libdir_map::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_grade_specific_files_for_grade(ProgressStream,
        Globals, NgsLibDirMap, GradeDir, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO) :-
    legacy_install_library_grade_specific_files_for_grade(ProgressStream,
        Globals, NgsLibDirMap, GradeDir, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO),
    globals.lookup_bool_option(Globals, experiment4, InstallProposed),
    (
        InstallProposed = no
    ;
        InstallProposed = yes,
        proposed_install_library_grade_specific_files_for_grade(ProgressStream,
            Globals, GradeDir, MainModuleName, AllModuleNames,
            !Succeeded, !Info, !IO)
    ).

%---------------------------------------------------------------------------%
%
% The code to install grade-specific files for the LEGACY install
% directory structure.
%

:- pred legacy_install_library_grade_specific_files_for_grade(
    io.text_output_stream::in, globals::in, libdir_map::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

legacy_install_library_grade_specific_files_for_grade(ProgressStream,
        Globals, NgsLibDirMap, GradeDir, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO) :-
    % This creates *some* of the directories into which we install
    % grade-specific files, but not all; some are created by the calls
    % to module_name_to_file_name_create_dirs below.
    legacy_make_grade_specific_install_dirs(ProgressStream, Globals, GradeDir,
        DirSucceeded, GsLibDirMap, !IO),
    (
        DirSucceeded = succeeded,
        globals.get_target(Globals, Target),
        get_std_grade_specific_install_lib_dir(Globals, GradeDir, GradeLibDir),
        (
            Target = target_csharp,
            ExtDll = ext_cur_gs(ext_cur_gs_lib_cil_dll),
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
            legacy_maybe_install_static_or_dynamic_archive(ProgressStream,
                Globals, sos_static, StaticLibFileName, GradeLibDir,
                !Succeeded, !IO),
            ( if StaticLibFileName = SharedLibFileName then
                true
            else
                legacy_maybe_install_static_or_dynamic_archive(ProgressStream,
                    Globals, sos_shared, SharedLibFileName, GradeLibDir,
                    !Succeeded, !IO)
            ),
            legacy_install_grade_init(ProgressStream, Globals, GradeDir,
                MainModuleName, !Succeeded, !IO)
        ),

        list.foldl3(
            legacy_install_grade_ints_and_headers(ProgressStream, Globals,
                NgsLibDirMap, GsLibDirMap, GradeDir),
            AllModuleNames, !Succeeded, !Info, !IO)
    ;
        DirSucceeded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ).

%---------------------%

:- pred legacy_make_grade_specific_install_dirs(io.text_output_stream::in,
    globals::in, string::in, maybe_succeeded::out, libdir_map::out,
    io::di, io::uo) is det.

legacy_make_grade_specific_install_dirs(ProgressStream, Globals, GradeDir,
        !:DirSucceeded, !:GsLibDirMap, !IO) :-
    !:DirSucceeded = succeeded,
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix / "lib" / "mercury",

    GradeIncSubDir = LibDir / "lib" / GradeDir / "inc",
    GradeIntsSubDir = LibDir / "ints" / GradeDir,
    GradeModuleSubDir = LibDir / "modules" / GradeDir,

    legacy_make_nonext_dir(ProgressStream, GradeIncSubDir / "Mercury",
        !DirSucceeded, !IO),
    legacy_make_nonext_dir(ProgressStream, GradeIntsSubDir / "Mercury",
        !DirSucceeded, !IO),
    legacy_make_nonext_dir(ProgressStream, GradeModuleSubDir,
        !DirSucceeded, !IO),

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
        legacy_make_ngs_dir_symlink_to_cur(ProgressStream, GradeIncSubDir,
            "mihs", !DirSucceeded, !GsLibDirMap, !IO),
        list.foldl3(
            legacy_make_ngs_dir_symlink_to_cur(ProgressStream,
                GradeIntsSubDir),
            ["opts", "analyses"],
            !DirSucceeded, !GsLibDirMap, !IO)
    ;
        UseSymLinks = no,
        legacy_make_ngs_dir(ProgressStream, GradeIncSubDir, "mihs",
            !DirSucceeded, !GsLibDirMap, !IO),
        legacy_make_ngs_dir(ProgressStream, GradeIntsSubDir, "opts",
            !DirSucceeded, !GsLibDirMap, !IO),
        legacy_make_ngs_dir(ProgressStream, GradeIntsSubDir, "analyses",
            !DirSucceeded, !GsLibDirMap, !IO)
    ).

%---------------------%

    % Install the `.init' file for the current grade.
    %
:- pred legacy_install_grade_init(io.text_output_stream::in, globals::in,
    string::in, module_name::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

legacy_install_grade_init(ProgressStream, Globals, GradeDir, MainModuleName,
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

%---------------------%

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred legacy_install_grade_ints_and_headers(io.text_output_stream::in,
    globals::in, libdir_map::in, libdir_map::in, string::in, module_name::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

legacy_install_grade_ints_and_headers(ProgressStream, Globals,
        NgsLibDirMap, GsLibDirMap, GradeDir, ModuleName,
        !Succeeded, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(_ModuleDepInfo),

        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix / "lib" / "mercury",

        % NOTE Before our ancestor
        % install_library_grade_specific_files_for_grade gets invoked,
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
            legacy_install_subdir_file(ProgressStream, Globals, GsLibDirMap,
                GradeIncDir, ModuleName, {ExtMih, "mihs"}, !Succeeded, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            %
            % XXX BUG Why are we installing to a NON-GRADE-SPECIFIC directory
            % in a predicate that does installs of GRADE-SPECIFIC files?
            % Any installs done by this code for one grade will be overwritten
            % by the install done by the next grade.
            IntsDir = LibDir / "ints",
            legacy_install_subdir_file(ProgressStream, Globals, NgsLibDirMap,
                IntsDir, ModuleName, {ExtMih, "mihs"}, !Succeeded, !IO)
        else
            true
        ),

        GradeIntsDir = LibDir / "ints" / GradeDir,
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            % XXX LEGACY
            ExtOpt = ext_cur_ngs_gs_max_ngs(
                ext_cur_ngs_gs_max_ngs_legacy_opt_plain),
            legacy_install_subdir_file(ProgressStream, Globals, GsLibDirMap,
                GradeIntsDir, ModuleName, {ExtOpt, "opts"}, !Succeeded, !IO)
        ;
            AnyIntermod = no
        ),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            ExtAn = ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
            legacy_install_subdir_file(ProgressStream, Globals, GsLibDirMap,
                GradeIntsDir, ModuleName, {ExtAn, "analyses"}, !Succeeded, !IO)
        ;
            IntermodAnalysis = no
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% The code to install grade-specific files for the PROPOSED install
% directory structure.
%

:- pred proposed_install_library_grade_specific_files_for_grade(
    io.text_output_stream::in, globals::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

proposed_install_library_grade_specific_files_for_grade(ProgressStream,
        Globals, Grade, MainModuleName, AllModuleNames,
        !Succeeded, !Info, !IO) :-
    gather_module_dep_infos(ProgressStream, Globals, AllModuleNames,
        _ModulesWithChildren, Succeeded, !Info, !IO),
    (
        Succeeded = did_not_succeed,
        !:Succeeded = did_not_succeed
    ;
        Succeeded = succeeded,

        globals.lookup_string_option(Globals, install_prefix, Prefix0),
        Prefix = Prefix0 / "MercurySystem",

        % This file type *is* be grade-specific, and should be listed
        % as such.
        ExtMD = ext_cur_ngs(ext_cur_ngs_misc_module_dep),
        proposed_install_all_gs_files(ProgressStream, Globals, Prefix, Grade,
            ExtMD, AllModuleNames, !Succeeded, !IO),

        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = no
        ;
            AnyIntermod = yes,
            ExtOpt = ext_cur_ngs_gs(ext_cur_ngs_gs_proposed_opt_plain),
            proposed_install_all_gs_files(ProgressStream, Globals,
                Prefix, Grade, ExtOpt, AllModuleNames, !Succeeded, !IO)
        ),

        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = no
        ;
            IntermodAnalysis = yes,
            ExtAn = ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
            proposed_install_all_gs_files(ProgressStream, Globals,
                Prefix, Grade, ExtAn, AllModuleNames, !Succeeded, !IO)
        ),

        globals.get_target(Globals, Target),
        (
            Target = target_c,
            proposed_install_library_grade_specific_files_for_grade_c(
                ProgressStream, Globals, Prefix, Grade,
                MainModuleName, AllModuleNames, !Succeeded, !IO)
        ;
            Target = target_java,
            proposed_install_library_grade_specific_files_for_grade_java(
                ProgressStream, Globals, Prefix, Grade,
                MainModuleName, AllModuleNames, !Succeeded, !IO)
        ;
            Target = target_csharp,
            proposed_install_library_grade_specific_files_for_grade_csharp(
                ProgressStream, Globals, Prefix, Grade,
                MainModuleName, AllModuleNames, !Succeeded, !IO)
        )
    ).

:- pred proposed_install_library_grade_specific_files_for_grade_c(
    io.text_output_stream::in, globals::in, string::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

proposed_install_library_grade_specific_files_for_grade_c(ProgressStream,
        Globals, Prefix, Grade, MainModuleName, AllModuleNames,
        !Succeeded, !IO) :-
    ExtInit = ext_cur_gs(ext_cur_gs_lib_init),
    proposed_install_all_gs_files(ProgressStream, Globals, Prefix, Grade,
        ExtInit, [MainModuleName], !Succeeded, !IO),

    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = no
    ;
        HighLevelCode = yes,
        ExtMih = ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih),
        proposed_install_all_gs_files(ProgressStream, Globals, Prefix, Grade,
            ExtMih, AllModuleNames, !Succeeded, !IO)
    ),

    ExtA =  ext_cur_gas_lib_lib_opt,
    ExtSo = ext_cur_gas_lib_sh_lib_opt,
    GenExtA =  ext_cur_gas(ExtA),
    GenExtSo = ext_cur_gas(ExtSo),
    module_name_to_workspace_lib_file_name(Globals, "lib", GenExtA,
        MainModuleName, StaticLibFileName),
    module_name_to_workspace_lib_file_name(Globals, "lib", GenExtSo,
        MainModuleName, SharedLibFileName),
    ext_cur_gas_extension_dir(Globals, ExtA,  _, StaticDirName),
    ext_cur_gas_extension_dir(Globals, ExtSo, _, SharedDirName),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    StaticInstallDir = Prefix / StaticDirName / Grade / TargetArch,
    SharedInstallDir = Prefix / SharedDirName / Grade / TargetArch,

    make_dir_handle_any_error(ProgressStream, StaticInstallDir,
        MakeStaticInstallDirSucceeded, !IO),
    (
        MakeStaticInstallDirSucceeded = did_not_succeed
    ;
        MakeStaticInstallDirSucceeded = succeeded,
        proposed_maybe_install_static_or_dynamic_archive(ProgressStream,
            Globals, sos_static, StaticInstallDir, StaticLibFileName,
            !Succeeded, !IO),
        ( if StaticLibFileName = SharedLibFileName then
            true
        else
            ( if StaticInstallDir = SharedInstallDir then
                MakeSharedInstallDirSucceeded = succeeded
            else
                make_dir_handle_any_error(ProgressStream, SharedInstallDir,
                    MakeSharedInstallDirSucceeded, !IO)
            ),
            (
                MakeSharedInstallDirSucceeded = did_not_succeed
            ;
                MakeSharedInstallDirSucceeded = succeeded,
                proposed_maybe_install_static_or_dynamic_archive(
                    ProgressStream, Globals, sos_shared,
                    SharedInstallDir, SharedLibFileName, !Succeeded, !IO)
            )
        )
    ).

:- pred proposed_install_library_grade_specific_files_for_grade_java(
    io.text_output_stream::in, globals::in, string::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

proposed_install_library_grade_specific_files_for_grade_java(ProgressStream,
        Globals, Prefix, Grade, MainModuleName, _AllModuleNames,
        !Succeeded, !IO) :-
    ExtJar = ext_cur_gs_lib_jar,
    GenExtJar = ext_cur_gs(ExtJar),
    module_name_to_workspace_file_name(Globals, GenExtJar,
        MainModuleName, JarFileName),
    ext_cur_gs_extension_dir(ExtJar, _,
        _LegacyJarDirName, ProposedJarDirName),
    JarInstallDir = Prefix / ProposedJarDirName / Grade,
    make_dir_handle_any_error(ProgressStream, JarInstallDir,
        MakeJarInstallDirSucceeded, !IO),
    (
        MakeJarInstallDirSucceeded = did_not_succeed
    ;
        MakeJarInstallDirSucceeded = succeeded,
        install_file_to(ProgressStream, Globals, JarInstallDir, JarFileName,
            !Succeeded, !IO)
    ).

:- pred proposed_install_library_grade_specific_files_for_grade_csharp(
    io.text_output_stream::in, globals::in, string::in, string::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

proposed_install_library_grade_specific_files_for_grade_csharp(ProgressStream,
        Globals, Prefix, Grade, MainModuleName, _AllModuleNames,
        !Succeeded, !IO) :-
    ExtCilDll = ext_cur_gs_lib_cil_dll,
    GenExtCilDll = ext_cur_gs(ExtCilDll),
    module_name_to_workspace_file_name(Globals, GenExtCilDll,
        MainModuleName, CilDllFileName),
    ext_cur_gs_extension_dir(ExtCilDll, _,
        _LegacyCilDllDirName, ProposedCilDllDirName),
    CilDllInstallDir = Prefix / ProposedCilDllDirName / Grade,
    make_dir_handle_any_error(ProgressStream, CilDllInstallDir,
        MakeCilDllInstallDirSucceeded, !IO),
    (
        MakeCilDllInstallDirSucceeded = did_not_succeed
    ;
        MakeCilDllInstallDirSucceeded = succeeded,
        install_file_to(ProgressStream, Globals, CilDllInstallDir,
            CilDllFileName, !Succeeded, !IO)
    ).

%---------------------%

:- type ext_cur_gs_ns =< ext
    --->    ext_cur_ngs(ext_cur_ngs)
    ;       ext_cur_gs(ext_cur_gs)
    ;       ext_cur_ngs_gs(ext_cur_ngs_gs)
    ;       ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur)
    ;       ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs).

:- pred proposed_install_all_gs_files(io.text_output_stream::in, globals::in,
    string::in, string::in, ext_cur_gs_ns::in, list(module_name)::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

proposed_install_all_gs_files(ProgressStream, Globals, Prefix, Grade,
        Ext, ModuleNames, !Succeeded, !IO) :-
    (
        Ext = ext_cur_ngs(ExtNgs),
        ext_cur_ngs_extension_dir(ExtNgs, _, ExtDirName)
    ;
        Ext = ext_cur_gs(ExtGs),
        ext_cur_gs_extension_dir(ExtGs, _, _, ExtDirName)
    ;
        Ext = ext_cur_ngs_gs(ExtNgsGs),
        ext_cur_ngs_gs_extension_dir(ExtNgsGs, _, ExtDirName)
    ;
        Ext = ext_cur_ngs_gs_max_cur(ExtNgsGsMaxCur),
        ext_cur_ngs_gs_max_cur_extension_dir(ExtNgsGsMaxCur, _, ExtDirName)
    ;
        Ext = ext_cur_ngs_gs_max_ngs(ExtNgsGsMaxNgs),
        ext_cur_ngs_gs_max_ngs_extension_dir(ExtNgsGsMaxNgs, _, ExtDirName)
    ),

    InstallDir = Prefix / ExtDirName / Grade,
    % XXX We can rely on the Prefix directory having been built
    % the install of the non-grade-specific files of the current grade.
    % Would this be more efficient if done by two calls make_single_directory,
    % adding ExtDirName and Grade respectively?
    make_dir_handle_any_error(ProgressStream, InstallDir,
        MakeInstallDirSucceeded, !IO),
    (
        MakeInstallDirSucceeded = did_not_succeed
    ;
        MakeInstallDirSucceeded = succeeded,
        GenExt = coerce(Ext),
        list.map(module_name_to_workspace_file_name(Globals, GenExt),
            ModuleNames, FileNames),
        install_files_to(ProgressStream, Globals, InstallDir,
            FileNames, !Succeeded, !IO)
    ).

%---------------------------------------------------------------------------%
% Utility predicates.
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% Utility predicates for use with the LEGACY install directory structure.
%

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
:- pred legacy_install_subdir_file(io.text_output_stream::in, globals::in,
    libdir_map::in, dir_name::in, module_name::in, {ext, string}::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

legacy_install_subdir_file(ProgressStream, Globals, LibDirMap, InstallDir,
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

:- pred legacy_maybe_install_static_or_dynamic_archive(
    io.text_output_stream::in, globals::in, static_or_shared::in,
    file_name::in, dir_name::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

legacy_maybe_install_static_or_dynamic_archive(ProgressStream, Globals,
        Linkage, FileName, InstallDir, !Succeeded, !IO) :-
    globals.get_library_install_linkages(Globals, LibLinkages),
    ( if set.member(Linkage, LibLinkages) then
        install_file(ProgressStream, Globals, FileName, InstallDir,
            succeeded, InstallSucceeded0, !IO),

        % We need to update the archive index after we copy it to the
        % installation directory, because the linkers on some OSs complain
        % if we don't.
        ( if
            Linkage = sos_static,
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

%---------------------------------------------------------------------------%

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

:- pred legacy_make_nonext_dir(io.text_output_stream::in, dir_name::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

legacy_make_nonext_dir(ProgressStream, DirName, !Succeeded, !IO) :-
    make_directory(DirName, IOResult, !IO),
    print_any_mkdir_error(ProgressStream, DirName, IOResult, !Succeeded, !IO).

:- pred legacy_make_ngs_dir(io.text_output_stream::in,
    dir_name::in, file_name::in, maybe_succeeded::in, maybe_succeeded::out,
    libdir_map::in, libdir_map::out, io::di, io::uo) is det.

legacy_make_ngs_dir(ProgressStream, CurDir, ExtDirName,
        !Succeeded, !LibDirMap, !IO) :-
    NgsDir = CurDir / "Mercury" / ExtDirName,
    make_directory(NgsDir, IOResult, !IO),
    print_any_mkdir_error(ProgressStream, NgsDir, IOResult, !Succeeded, !IO),
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
:- pred legacy_make_ngs_dir_symlink_to_cur(io.text_output_stream::in,
    dir_name::in, file_name::in, maybe_succeeded::in, maybe_succeeded::out,
    libdir_map::in, libdir_map::out, io::di, io::uo) is det.

legacy_make_ngs_dir_symlink_to_cur(ProgressStream, CurDir, ExtDirName,
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
        legacy_make_ngs_dir(ProgressStream, CurDir, ExtDirName,
            !Succeeded, !LibDirMap, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates for use with the PROPOSED install directory structure.
%

:- pred gather_module_dep_infos(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

gather_module_dep_infos(ProgressStream, Globals, AllModuleNames,
        ModulesWithChildren, Succeeded, !Info, !IO) :-
    gather_module_dep_infos_loop(ProgressStream, Globals, AllModuleNames,
        cord.init, ModulesWithChildrenCord,
        cord.init, ModulesWithoutDepInfoCord, !Info, !IO),
    ModulesWithChildren = cord.list(ModulesWithChildrenCord),
    ModulesWithoutDepInfo = cord.list(ModulesWithoutDepInfoCord),
    (
        ModulesWithoutDepInfo = [],
        Succeeded = succeeded
    ;
        ModulesWithoutDepInfo = [_ | _],
        % XXX The LEGACY install process does not print an error message
        % for this error. The PROPOSED process should, but what should
        % the message say?
        Succeeded = did_not_succeed
    ).

:- pred gather_module_dep_infos_loop(io.text_output_stream::in, globals::in,
    list(module_name)::in,
    cord(module_name)::in, cord(module_name)::out,
    cord(module_name)::in, cord(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

gather_module_dep_infos_loop(_ProgressStream, _Globals, [],
        !ModulesWithChildren, !ModulesWithoutDepInfo, !Info, !IO).
gather_module_dep_infos_loop(ProgressStream, Globals,
        [ModuleName | ModuleNames],
        !ModulesWithChildren, !ModulesWithoutDepInfo, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = no_module_dep_info,
        cord.snoc(ModuleName, !ModulesWithoutDepInfo)
    ;
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_children(ModuleDepInfo, Children),
        ( if set.is_non_empty(Children) then
            cord.snoc(ModuleName, !ModulesWithChildren)
        else
            true
        )
    ),
    gather_module_dep_infos_loop(ProgressStream, Globals, ModuleNames,
        !ModulesWithChildren, !ModulesWithoutDepInfo, !Info, !IO).

%---------------------%

:- pred proposed_maybe_install_static_or_dynamic_archive(
    io.text_output_stream::in, globals::in, static_or_shared::in,
    dir_name::in, file_name::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

proposed_maybe_install_static_or_dynamic_archive(ProgressStream, Globals,
        Linkage, InstallDir, FileName, !Succeeded, !IO) :-
    globals.get_library_install_linkages(Globals, LibLinkages),
    ( if set.member(Linkage, LibLinkages) then
        install_file_to(ProgressStream, Globals, InstallDir, FileName,
            succeeded, InstallSucceeded0, !IO),
        % We need to update the archive index after we copy it to the
        % installation directory, because the linkers on some OSs complain
        % if we don't.
        ( if
            Linkage = sos_static,
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

%---------------------%

:- pred install_files_to(io.text_output_stream::in, globals::in,
    dir_name::in, list(file_name)::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

install_files_to(ProgressStream, Globals, InstallDir, FileNames,
        !Succeeded, !IO) :-
    % XXX This code copies files to InstallDir one at a time.
    % With install_method_external_cmd, this will incur all the overhead
    % of invoke_system_command N times, where N is the number of module
    % names in ModuleNames. That overhead will include
    %
    % - the creation of a shell process,
    % - the creation of the process that does the copying (e.g. cp)
    % - the creation, reading and removal of a temp file
    %   for storing the output of the process that does the copying.
    %
    % It would be more efficient if we invoked a *single* shell command
    % to copy *all* FileNames to InstallDir.
    %
    % There are two potential flaws in this plan.
    %
    % - If FileNames is long enough, the length of the copy command
    %   string may exceed OS limits. We can work around such limits by
    %   using xargs-style chunking.
    %
    % - When would we print the "Installing <filename>" progress message?
    %   The answer does not matter in the absence of errors, but in their
    %   presence, the only non-misleading option is to print a single
    %   "Installing <filename1> <filename2> ..." message just as
    %   we are about to install a chunk of filenames. That would be
    %   a challenge to format in a readable but still non-misleading way.
    list.foldl2(install_file_to(ProgressStream, Globals, InstallDir),
        FileNames, !Succeeded, !IO).

%---------------------%

    % Look up the filename under which the given module's file with the given
    % extension is stored in the current workspace.
    %
:- pred module_name_to_workspace_file_name(globals::in,
    ext::in, module_name::in, file_name::out) is det.

module_name_to_workspace_file_name(Globals, Ext, ModuleName, FileName) :-
    % XXX LEGACY For a transition period, we are copying from
    % workspaces that have the LEGACY directory structure.
    %
    % Switching over will require
    %
    % - either a "flag day", where you do a realclean of your workspace,
    %   switch to a compiler install that uses the PROPOSED structure
    %   for workspaces as well as for installs, and rebuilding, or
    % - switching from a workspace that uses --no-use-subdirs, in which
    %   the LEGACY and PROPOSED schemes both store all files in the same
    %   directory (the current directory).
    module_name_to_file_name(Globals, $pred, Ext, ModuleName,
        FileName, _FileNameProposed).

    % This is the library version of module_name_to_workspace_file_name.
    %
:- pred module_name_to_workspace_lib_file_name(globals::in, string::in,
    ext::in, module_name::in, file_name::out) is det.

module_name_to_workspace_lib_file_name(Globals, LibPrefix, Ext,
        MainModuleName, LibFileName) :-
    % XXX LEGACY The comment in module_name_to_workspace_file_name
    % applies here as well.
    module_name_to_lib_file_name(Globals, $pred, LibPrefix, Ext,
        MainModuleName, LibFileName, _LibFileNameProposed).

%---------------------------------------------------------------------------%
%
% Utility predicates that should work with both the LEGACY and the PROPOSED
% install directory structure.
%

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

%---------------------%

:- pred remove_target_file_if_grade_dependent(target_id::in, target_status::in,
    version_hash_table(target_id, target_status)::in,
    version_hash_table(target_id, target_status)::out) is det.

remove_target_file_if_grade_dependent(TargetId, _Status, !StatusMap) :-
    ( if
        TargetId = merc_target(target_file(_, TargetType)),
        is_target_grade_dependent(TargetType) = grade_dependent
    then
        version_hash_table.delete(TargetId, !StatusMap)
    else
        true
    ).

%---------------------%

:- pred make_dir_handle_any_error(io.text_output_stream::in, dir_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

make_dir_handle_any_error(ProgressStream, DirName, Succeeded, !IO) :-
    dir.make_directory(DirName, MakeDirResult, !IO),
    print_any_mkdir_error(ProgressStream, DirName, MakeDirResult,
        succeeded, Succeeded, !IO).

:- pred print_any_mkdir_error(io.text_output_stream::in, dir_name::in,
    io.res::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

print_any_mkdir_error(ProgressStream, DirName, Result, !Succeeded, !IO) :-
    (
        Result = ok
    ;
        Result = error(Error),
        print_mkdir_error(ProgressStream, DirName, Error, !:Succeeded, !IO)
    ).

:- pred print_mkdir_error(io.text_output_stream::in, dir_name::in,
    io.error::in, maybe_succeeded::out, io::di, io::uo) is det.

print_mkdir_error(ProgressStream, DirName, Error, !:Succeeded, !IO) :-
    ErrorMsg = io.error_message(Error),
    io.format(ProgressStream,
        "Error creating installation directory %s: %s\n",
        [s(DirName), s(ErrorMsg)], !IO),
    !:Succeeded = did_not_succeed.

%---------------------------------------------------------------------------%
:- end_module make.library_install.
%---------------------------------------------------------------------------%
