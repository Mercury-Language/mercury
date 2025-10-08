%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2009 The University of Melbourne.
% Copyright (C) 2015-2016, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks if all the Mercury libraries needed to build a target
% are installed in the required grade.
%
% XXX The maybe_check_libraries_are_installed predicate has always been
% in this module, while the other exported predicates and their supporting
% code have been moved here relatively recently. It should be possible
% to factor out code that is common to both of these halves of this module,
% but this has not yet been done.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module libs.check_libgrades.
:- interface.

:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % This predicate answers the question: given the specified location
    % of the Mercury standard library, in which grades is the Mercury standard
    % library installed there?
    %
    % The location of the Mercury standard library may be specified by
    %
    % - the value of the mercury_standard_library_directory option,
    % - the value of the MERCURY_STDLIB_DIR make variable in an options file,
    %   or
    % - the value of the MERCURY_STDLIB_DIR environment variable.
    %
    % The above is the priority order of the sources. The second and third
    % sources are both handled by options_file.m.
    %
    % Note that mmc needs to know the set of installed grades only during
    % a few kinds of mmc invocations, these being
    %
    % - invocations that install a user-written library with mmc --make, and
    % - invocations of mmc --output-libgrades.
    %
    % Note that first kind of invocations want to know the set of grades
    % in which the Mercury standard library is installed because they want to
    % install the *user* library in the same set of grades.
    %
    % Installing a user library using mmake (*not* mmc --make) works
    % differently, because in that case, it is mmake that has to decide
    % the set of grades we want to install. That decision is currently done
    % by having the configure script put the configured set of grades to
    % install into the Mmake.vars file as the value of $(LIBGRADES).
    %
    % This method is less flexible than the one used by mmc --make, in that
    % only the latter can handle having the Mercury standard library installed
    % in one or more new grades *after* configuration.
    %
    % XXX We should probably change installs using mmake to invoke
    % "mmc --output-libgrades" instead of consulting the configured set
    % of grades in Mmake.vars. However, we can do that *only* if we
    % separate out two related but nevertheless distinct notions:
    %
    % - the set of grades in which we should install the Mercury standard
    %   library, and
    %
    % - the set of grades in which we should install any other library.
    %
    % The distinction is important because mmc--output-libgrades works
    % by looking for installed grades, and there simply won't be any
    % when Mercury is being installed on a machine for the first time.
    % In that case, we will still need to know the configured set of grades.
    %
:- pred detect_stdlib_grades(io.text_output_stream::in, globals::in,
    maybe1(set(string))::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % This predicate answers two questions, the first of which is related to
    % but nevertheless quite different from the question above: given the
    % specified location of the Mercury standard library, is the Mercury
    % standard library installed there *in the grade given by the user*,
    % which is now in Globals?
    %
    % The second question is totally unrelated: are the libraries named by the
    % mercury_libraries option installed in the grade given by Globals in the
    % directories named by the mercury_library_directories and
    % init_file_directories options?
    %
    % We answer the questions above only if --libgrade-install-check is
    % enabled. If --libgrade-install-check is *not* enabled, we always return
    % an empty list of errors.
    %
:- pred maybe_check_libraries_are_installed(globals::in, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module parse_tree.find_module.

:- import_module bool.
:- import_module dir.
:- import_module io.file.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

detect_stdlib_grades(ProgressStream, Globals, MaybeStdLibGrades, !IO) :-
    % Enable the compile-time trace flag "debug-detect-libgrades" to enable
    % debugging messages for library grade detection in the very verbose
    % output.
    globals.lookup_bool_option(Globals, verbose, Verbose),
    trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
        maybe_write_string(ProgressStream, Verbose,
            "% Detecting library grades ...\n", !TIO)
    ),
    globals.lookup_maybe_string_option(Globals,
        chosen_stdlib_dir, MaybeChosenStdLibDir),
    check_chosen_stdlib_dir_exists(MaybeChosenStdLibDir,
        MaybeMerStdLibDir, !IO),
    (
        MaybeMerStdLibDir = ok1(MerStdLibDir),
        trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
            (
                Verbose = yes,
                set.fold(report_detected_libgrade(ProgressStream),
                    StdLibGrades, !TIO),
                io.write_string(ProgressStream, "% done.\n", !TIO)
            ;
                Verbose = no
            )
        ),
        do_detect_libgrades(MerStdLibDir, StdLibGrades, !IO),
        MaybeStdLibGrades = ok1(StdLibGrades)
    ;
        MaybeMerStdLibDir = error1(Specs),
        trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
            maybe_write_string(ProgressStream, Verbose, "% failed.\n", !TIO)
        ),
        MaybeStdLibGrades = error1(Specs)
    ).

:- pred can_you_read_dir(string::in, maybe1(string)::out, io::di, io::uo)
    is det.

can_you_read_dir(MerStdLibDir, MaybeMerStdLibDir, !IO) :-
    io.file.check_file_accessibility(MerStdLibDir, [read], CanRead, !IO),
    (
        CanRead = ok,
        MaybeMerStdLibDir = ok1(MerStdLibDir)
    ;
        CanRead = error(ReadError),
        io.error_message(ReadError, ReadErrorMsg),
        Pieces = [words("Error:"), fixed(MerStdLibDir), suffix(":"), nl,
            words(ReadErrorMsg), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options, Pieces),
        MaybeMerStdLibDir = error1([Spec])
    ).

:- pred do_detect_libgrades(string::in, set(string)::out,
    io::di, io::uo) is det.

do_detect_libgrades(StdLibDir, Grades, !IO) :-
    % XXX LEGACY
    ModulesDir = StdLibDir / "modules",
    dir.foldl2(do_detect_libgrade_using_init_file, ModulesDir,
        set.init, MaybeGrades0, !IO),
    (
        MaybeGrades0 = ok(Grades0),
        LibsDir = StdLibDir / "lib",
        dir.foldl2(do_detect_libgrade_using_lib_file, LibsDir,
            Grades0, MaybeGrades, !IO),
        (
            MaybeGrades = ok(Grades)
        ;
            MaybeGrades = error(_, _),
            set.init(Grades)
        )
    ;
        MaybeGrades0 = error(_, _),
        set.init(Grades)
    ).

    % Test for the presence of an installed grade by looking for mer_std.init.
    % This works for C grades, but not for C# or Java grades.
    %
:- pred do_detect_libgrade_using_init_file(string::in, string::in,
    io.file_type::in, bool::out, set(string)::in, set(string)::out,
    io::di, io::uo) is det.

do_detect_libgrade_using_init_file(DirName, GradeFileName,
        GradeFileType, Continue, !Grades, !IO) :-
    (
        GradeFileType = directory,
        % XXX LEGACY
        InitFile = DirName / GradeFileName / "mer_std.init",
        io.file.check_file_accessibility(InitFile, [read], Result, !IO),
        (
            Result = ok,
            set.insert(GradeFileName, !Grades)
        ;
            Result = error(_)
        )
    ;
        ( GradeFileType = regular_file
        ; GradeFileType = symbolic_link
        ; GradeFileType = named_pipe
        ; GradeFileType = socket
        ; GradeFileType = character_device
        ; GradeFileType = block_device
        ; GradeFileType = message_queue
        ; GradeFileType = semaphore
        ; GradeFileType = shared_memory
        ; GradeFileType = unknown
        )
    ),
    Continue = yes.

    % Test for the presence of installed Java and C# grades by looking for
    % the standard library's .jar or .dll file respectively.
    %
:- pred do_detect_libgrade_using_lib_file(string::in, string::in,
    io.file_type::in, bool::out, set(string)::in, set(string)::out,
    io::di, io::uo) is det.

do_detect_libgrade_using_lib_file(DirName, GradeFileName, GradeFileType,
        Continue, !Grades, !IO) :-
    (
        GradeFileType = directory,
        ( if
            csharp_or_java_libgrade_target(GradeFileName, LibFile)
        then
            TargetFile = DirName / GradeFileName / LibFile,
            io.file.check_file_accessibility(TargetFile, [read], Result, !IO),
            (
                Result = ok,
                set.insert(GradeFileName, !Grades)
            ;
                Result = error(_)
            )
        else
            true
        )
    ;
        ( GradeFileType = regular_file
        ; GradeFileType = symbolic_link
        ; GradeFileType = named_pipe
        ; GradeFileType = socket
        ; GradeFileType = character_device
        ; GradeFileType = block_device
        ; GradeFileType = message_queue
        ; GradeFileType = semaphore
        ; GradeFileType = shared_memory
        ; GradeFileType = unknown
        )
    ),
    Continue = yes.

:- pred csharp_or_java_libgrade_target(string::in, string::out) is semidet.

csharp_or_java_libgrade_target(GradeFileName, LibFile) :-
    ( if string.prefix(GradeFileName, "csharp") then
        LibFile = "mer_std.dll"
    else if string.prefix(GradeFileName, "java") then
         LibFile = "mer_std.jar"
    else
        false
    ).

:- pred report_detected_libgrade(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

report_detected_libgrade(Stream, Grade, !IO) :-
    io.format(Stream, "%% Detected library grade: %s\n", [s(Grade)], !IO).

%---------------------------------------------------------------------------%

    % This mutable is a cache that records whether the job of
    % maybe_check_libraries_are_installed has already been done
    % for a given set of parameters.
    %
    % I (zs) think that this cache is useful mostly when mmc --make
    % needs to compile more than one module. Those compilations are
    % almost always done with the same parameters, and so
    %
    % - there is no point in repeating the test, since the same inputs
    %   are guaranteed to yield the same outputs, while
    %
    % - there is point in avoiding tests when this can be done safely,
    %   because the file operations done by the test are expensive.
    %
:- mutable(has_check_libraries_been_done, check_libraries_maybe_done,
    check_libraries_not_done, ground,
    [untrailed, attach_to_io_state, thread_local]).

    % This is the type of the values in that cache.
    %
:- type check_libraries_maybe_done
    --->    check_libraries_not_done
            % We have not yet called check_libraries_are_installed.
    ;       check_libraries_done(
                % We have called check_libraries_are_installed with these
                % values of Target, GradeDirName, MaybeStdLibDir,
                % MercuryLibDirs, InitFileDirs and Libs ...
                check_libraries_inputs,

                % ... and result was this.
                list(error_spec)
            ).

:- type check_libraries_inputs
    --->    check_libraries_inputs(
                cli_target              :: compilation_target,
                cli_grade_dir_name      :: string,
                cli_chosen_stdlib_dir   :: maybe(string),
                cli_mercury_lib_dirs    :: list(string),
                cli_init_file_dirs      :: list(string),
                cli_named_libs          :: list(string)
            ).

maybe_check_libraries_are_installed(Globals, Specs, !IO) :-
    globals.lookup_bool_option(Globals, libgrade_install_check, LibgradeCheck),
    (
        LibgradeCheck = yes,
        % Get all the components of Globals that are inputs to our job,
        % so we can make sure that we use any cached result only if its
        % actually applicable.
        globals.get_target(Globals, Target),
        globals.get_grade_dir(Globals, GradeDirName),
        globals.lookup_maybe_string_option(Globals,
            chosen_stdlib_dir, MaybeChosenStdLibDir),
        globals.lookup_accumulating_option(Globals,
            mercury_library_directories, MercuryLibDirs),
        globals.lookup_accumulating_option(Globals, init_file_directories,
            InitFileDirs),
        globals.lookup_accumulating_option(Globals, mercury_libraries,
            NamedLibs),
        Inputs = check_libraries_inputs(Target, GradeDirName,
            MaybeChosenStdLibDir, MercuryLibDirs, InitFileDirs, NamedLibs),

        get_has_check_libraries_been_done(Cache0, !IO),
        ( if
            Cache0 = check_libraries_done(CacheInputs, CacheSpecs),
            Inputs = CacheInputs
        then
            Specs = CacheSpecs
        else
            check_stdlib_is_installed(Inputs, Specs0, !IO),
            check_named_libraries_are_installed(Globals, Inputs, NamedLibs,
                Specs0, Specs, !IO),
            Cache = check_libraries_done(Inputs, Specs),
            set_has_check_libraries_been_done(Cache, !IO)
        )
    ;
        LibgradeCheck = no,
        Specs = []
    ).

%---------------------------------------------------------------------------%

    % Part 1 of the maybe_check_libraries_are_installed test:
    % is the Mercury standard library installed?
    %
:- pred check_stdlib_is_installed(check_libraries_inputs::in,
    list(error_spec)::out, io::di, io::uo) is det.

check_stdlib_is_installed(Inputs, Specs, !IO) :-
    MaybeChosenStdLibDir = Inputs ^ cli_chosen_stdlib_dir,
    check_chosen_stdlib_dir_exists(MaybeChosenStdLibDir,
        MaybeMerStdLibDir, !IO),
    (
        MaybeMerStdLibDir = ok1(StdLibDir),
        Target = Inputs ^ cli_target,
        GradeDirName = Inputs ^ cli_grade_dir_name,
        % XXX LEGACY
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
            Specs = []
        ;
            StdLibCheckFileResult = error(_),
            io.progname_base("mercury_compile", ProgName, !IO),
            Pieces = [fixed(ProgName), suffix(":"), words("error:"),
                words("the Mercury standard library cannot be found"),
                words("in grade"), quote(GradeDirName), suffix("."), nl],
            Spec = no_ctxt_spec($pred, severity_error,
                phase_check_libs, Pieces),
            Specs = [Spec]
        )
    ;
        MaybeMerStdLibDir = error1(Specs)
    ).

%---------------------------------------------------------------------------%

    % Part 2 of the maybe_check_libraries_are_installed test:
    % are libraries named in the mercury_libraries option installed?
    %
:- pred check_named_libraries_are_installed(globals::in,
    check_libraries_inputs::in, list(string)::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_named_libraries_are_installed(_, _, [], !Specs, !IO).
check_named_libraries_are_installed(Globals, Inputs, [LibName | LibNames],
        !Specs, !IO) :-
    check_named_library_is_installed(Globals, Inputs, LibName,
        !Specs, !IO),
    check_named_libraries_are_installed(Globals, Inputs, LibNames,
        !Specs, !IO).

:- pred check_named_library_is_installed(globals::in,
    check_libraries_inputs::in, string::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_named_library_is_installed(Globals, Inputs, LibName, !Specs, !IO) :-
    % NOTE This predicate must not use Globals for any purpose except
    % getting search authorizations. For every other purpose, we should get
    % our ancestor maybe_check_libraries_are_installed to look things up,
    % and pass us the looked-up value in Inputs. This is needed to ensure
    % that we don't use cached check results inappropriately.
    %
    % XXX We should probably include the options whose use is implied
    % in our search authorizations in the check_libraries_inputs structure
    % as well. However, the absence of the options used to search LEGACY
    % install directories have not been missed for a long time. On the
    % other hand, we should include the options used to search PROPOSED
    % install directories should be added only when we start using them.
    globals.get_target(Globals, Target),
    (
        % In C grades, check for the presence of a library by seeing
        % if its .init files exists.
        Target = target_c,
        TestFileName = LibName ++ ".init",
        % NOTE: we don't look up the value of the option init_files here
        % because that may include .init files other than those associated with
        % any libraries.
        StdLibExt = sle_init
    ;
        % In Java grades, check for the presence of the JAR for library.
        Target = target_java,
        TestFileName = LibName ++ ".jar",
        StdLibExt = sle_jar
    ;
        % In C# grades, check for the presence of the DLL for the library.
        Target = target_csharp,
        TestFileName = LibName ++ ".dll",
        StdLibExt = sle_dll
    ),
    SearchAuthDirs = get_search_auth_stdlib_dirs(StdLibExt, Globals),
    search_for_file_returning_dir(SearchAuthDirs, TestFileName,
        _SearchDirs, MaybeDirName, !IO),
    (
        MaybeDirName = ok(_)
    ;
        MaybeDirName = error(_),
        io.progname_base("mercury_compile", ProgName, !IO),
        GradeDirName = Inputs ^ cli_grade_dir_name,
        % XXX SEARCH_ERROR TestFileName, _SearchDirs
        Pieces = [fixed(ProgName), suffix(":"), words("error:"),
            words("the library"), quote(LibName), words("cannot be found"),
            words("in grade"), quote(GradeDirName), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_check_libs, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Is the Mercury standard library where we think it is?
    %
:- pred check_chosen_stdlib_dir_exists(maybe(string)::in, maybe1(string)::out,
    io::di, io::uo) is det.

check_chosen_stdlib_dir_exists(MaybeChosenStdLibDir, MaybeMerStdLibDir, !IO) :-
    (
        MaybeChosenStdLibDir = yes(ChosenStdLibDir),
        can_you_read_dir(ChosenStdLibDir, MaybeMerStdLibDir, !IO)
    ;
        MaybeChosenStdLibDir = no,
        % The error message should have already been generated by
        % handle_directory_options in handle_options.m.
        MaybeMerStdLibDir = error1([])
    ).

%---------------------------------------------------------------------------%
:- end_module libs.check_libgrades.
%---------------------------------------------------------------------------%
