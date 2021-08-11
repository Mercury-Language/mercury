%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2009 The University of Melbourne.
% Copyright (C) 2015-2016, 2019 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks if all the Mercury libraries needed to build a target
% are installed in the required grade.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module libs.check_libgrades.
:- interface.

:- import_module libs.globals.
:- import_module libs.process_util.

:- import_module io.

%---------------------------------------------------------------------------%

    % If --libgrade-install-check is enabled, then check that all Mercury
    % libraries required by the target are installed in the selected grade.
    % Always succeeds if --libgrade-install-check is *not* enabled.
    %
:- pred maybe_check_libraries_are_installed(globals::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.file_util.
:- import_module libs.options.

:- import_module bool.
:- import_module dir.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

maybe_check_libraries_are_installed(Globals, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, libgrade_install_check,
        LibgradeCheck),
    (
        LibgradeCheck = yes,
        globals.lookup_accumulating_option(Globals, mercury_libraries, Libs),
        grade_directory_component(Globals, GradeDirName),
        check_stdlib_is_installed(Globals, GradeDirName, Succeeded0, !IO),
        list.foldl2(check_library_is_installed(Globals, GradeDirName), Libs,
            Succeeded0, Succeeded, !IO)
    ;
        LibgradeCheck = no,
        Succeeded = succeeded
    ).

:- pred check_stdlib_is_installed(globals::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

check_stdlib_is_installed(Globals, GradeDirName, Succeeded, !IO) :-
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            % In C grades, check for the presence of mer_std.init in the
            % required grade. Unless the installation is broken, this implies
            % the presence of the other standard library files in that grade.
            StdLibCheckFile =
                StdLibDir / "modules" / GradeDirName / "mer_std.init"
        ;
            % Java grades do not use .init files, so check for the presence of
            % the standard library JAR.
            Target = target_java,
            StdLibCheckFile = StdLibDir / "lib" / GradeDirName / "mer_std.jar"
        ;
            % C# grades do not use .init files, so check for the presence of
            % the standard library DLL.
            Target = target_csharp,
            StdLibCheckFile = StdLibDir / "lib" / GradeDirName / "mer_std.dll"
        ),
        io.open_input(StdLibCheckFile, StdLibCheckFileResult, !IO),
        (
            StdLibCheckFileResult = ok(StdLibCheckFileStream),
            io.close_input(StdLibCheckFileStream, !IO),
            Succeeded = succeeded
        ;
            StdLibCheckFileResult = error(_),
            % XXX It would be better for our *caller* to print this kind of
            % message, since it may know a more appropriate target stream
            % than stderr.
            io.stderr_stream(Stderr, !IO),
            io.progname_base("mercury_compile", ProgName, !IO),
            io.format(Stderr,
                "%s: error: the Mercury standard library "  ++
                "cannot be found in grade %s.\n",
                [s(ProgName), s(GradeDirName)], !IO),
            Succeeded = did_not_succeed
        )
    ;
        MaybeStdLibDir = no,
        Succeeded = succeeded
    ).

:- pred check_library_is_installed(globals::in, string::in,
    string::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

check_library_is_installed(Globals, GradeDirName, LibName, !Succeeded, !IO) :-
    globals.get_target(Globals, Target),
    (
        % In C grades, check for the presence of a library by seeing
        % if its .init files exists.
        Target = target_c,
        CheckFileName = LibName ++ ".init",
        % NOTE: we don't look up the value of the option init_files here
        % because that may include .init files other than those associated with
        % any libraries.
        globals.lookup_accumulating_option(Globals, init_file_directories,
            SearchDirs)
    ;
        (
            % In Java grades, check for the presence of the JAR for library.
            Target = target_java,
            CheckFileName = LibName ++ ".jar"
        ;
            % In C# grades, check for the presence of the DLL for the library.
            Target = target_csharp,
            CheckFileName = LibName ++ ".dll"
        ),
        globals.lookup_accumulating_option(Globals,
            mercury_library_directories, MercuryLibDirs),
        grade_directory_component(Globals, GradeDirNameDir),
        SearchDirs = list.map(
            (func(LibDir) = LibDir / "lib" / GradeDirNameDir),
            MercuryLibDirs)
    ),
    search_for_file_returning_dir(SearchDirs, CheckFileName, MaybeDirName,
        !IO),
    (
        MaybeDirName = ok(_)
    ;
        MaybeDirName = error(_),
        io.stderr_stream(Stderr, !IO),
        io.progname_base("mercury_compile", ProgName, !IO),
        io.format(Stderr,
            "%s: error: the library `%s' cannot be found in grade `%s'.\n",
            [s(ProgName), s(LibName), s(GradeDirName)], !IO),
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
:- end_module libs.check_libgrades.
%---------------------------------------------------------------------------%
