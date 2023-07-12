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
:- import_module libs.options.
:- import_module make.
:- import_module make.options_file.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- pred maybe_detect_stdlib_grades(option_table::in, options_variables::in,
    maybe1(set(string))::out, list(string)::out, io::di, io::uo) is det.

    % Where is the Mercury standard library?
    %
    % NOTE: A standard library directory specified on the command line
    % overrides one set using the MERCURY_STDLIB_DIR variable.
    %
:- pred find_mercury_stdlib(option_table::in, options_variables::in,
    maybe1(string)::out, io::di, io::uo) is det.

:- pred do_detect_libgrades(string::in, set(string)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % If --libgrade-install-check is enabled, then check that all Mercury
    % libraries required by the target are installed in the selected grade.
    % Always succeeds if --libgrade-install-check is *not* enabled.
    %
:- pred maybe_check_libraries_are_installed(globals::in, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compute_grade.
:- import_module libs.file_util.
:- import_module parse_tree.find_module.

:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module io.file.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

maybe_detect_stdlib_grades(OptionTable, Variables,
        MaybeStdlibGrades, StdlibGradeOpts, !IO) :-
    % Enable the compile-time trace flag "debug-detect-libgrades" to enable
    % debugging messages for library grade detection in the very verbose
    % output.
    getopt.lookup_bool_option(OptionTable, detect_libgrades, Detect),
    (
        Detect = yes,
        io.stdout_stream(StdOut, !IO),
        getopt.lookup_bool_option(OptionTable, verbose, Verbose),
        trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
            maybe_write_string(StdOut, Verbose,
                "% Detecting library grades ...\n", !TIO)
        ),
        find_mercury_stdlib(OptionTable, Variables, MaybeMerStdLibDir, !IO),
        % If we cannot find the Mercury standard library directory,
        % we return the error message(s) explaining the reason for that
        % in MaybeStdlibGrades, which one our caller pays attention to,
        % but NOT in StdlibGradeOpts, which is for another of our callers.
        (
            MaybeMerStdLibDir = ok1(MerStdLibDir),
            do_detect_libgrades(MerStdLibDir, StdlibGrades, !IO),
            MaybeStdlibGrades = ok1(StdlibGrades)
        ;
            MaybeMerStdLibDir = error1(Specs),
            MaybeStdlibGrades = error1(Specs),
            set.init(StdlibGrades)
        ),
        trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
            (
                Verbose = yes,
                set.fold(report_detected_libgrade(StdOut), StdlibGrades, !TIO),
                io.write_string(StdOut, "% done.\n", !TIO)
            ;
                Verbose = no
            )
        )
    ;
        Detect = no,
        set.init(StdlibGrades),
        MaybeStdlibGrades = ok1(StdlibGrades)
    ),
    GradeToOpts = (func(Grade) = ["--libgrade", Grade]),
    StdlibGradeOptionPairs =
        list.map(GradeToOpts, set.to_sorted_list(StdlibGrades)),
    list.condense(StdlibGradeOptionPairs, StdlibGradeOpts).

find_mercury_stdlib(OptionTable, Variables, MaybeMerStdLibDir, !IO) :-
    ( if
        % Was the standard library directory set on the command line?
        getopt.lookup_maybe_string_option(OptionTable,
            mercury_standard_library_directory, MaybeStdLibDir),
        MaybeStdLibDir = yes(MerStdLibDir)
    then
        can_you_read_dir(MerStdLibDir, MaybeMerStdLibDir, !IO)
    else
        % Was the standard library directory set using the
        % MERCURY_STDLIB_DIR variable?
        lookup_mercury_stdlib_dir(Variables, MaybeConfigMerStdLibDir),
        (
            MaybeConfigMerStdLibDir = ok1(MerStdLibDirs),
            (
                MerStdLibDirs = [MerStdLibDir],
                can_you_read_dir(MerStdLibDir, MaybeMerStdLibDir, !IO)
            ;
                MerStdLibDirs = [],
                Pieces = [words("Error: the location of the directory"),
                    words("that holds the Mercury standard library"),
                    words("is not specified either by the value of any"),
                    quote("--mercury-stdlib-dir"), words("option"),
                    words("to the compiler, nor by any definition of the"),
                    quote("MERCURY_STDLIB_DIR"), words("variable in the"),
                    quote("Mercury.config"), words("file."), nl],
                Spec = simplest_no_context_spec($pred, severity_error,
                    phase_options, Pieces),
                MaybeMerStdLibDir = error1([Spec])
            ;
                MerStdLibDirs = [_, _ | _],
                Pieces = [words("Error: the definition of the"),
                    quote("MERCURY_STDLIB_DIR"), words("variable in the"),
                    quote("Mercury.config"), words("file"),
                    words("contains more than one string."), nl],
                Spec = simplest_no_context_spec($pred, severity_error,
                    phase_options, Pieces),
                MaybeMerStdLibDir = error1([Spec])
            )
        ;
            MaybeConfigMerStdLibDir = error1(Specs),
            MaybeMerStdLibDir = error1(Specs)
        )
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
        Spec = simplest_no_context_spec($pred, severity_error,
            phase_options, Pieces),
        MaybeMerStdLibDir = error1([Spec])
    ).

do_detect_libgrades(StdLibDir, Grades, !IO) :-
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
    % This works for all grades except the C# and Java grades.
    %
:- pred do_detect_libgrade_using_init_file(string::in, string::in,
    io.file_type::in, bool::out, set(string)::in, set(string)::out,
    io::di, io::uo) is det.

do_detect_libgrade_using_init_file(DirName, GradeFileName,
        GradeFileType, Continue, !Grades, !IO) :-
    (
        GradeFileType = directory,
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
    % the standard library JAR or assembly respectively.
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

maybe_check_libraries_are_installed(Globals, !:Specs, !IO) :-
    globals.lookup_bool_option(Globals, libgrade_install_check,
        LibgradeCheck),
    (
        LibgradeCheck = yes,
        globals.lookup_accumulating_option(Globals, mercury_libraries, Libs),
        grade_directory_component(Globals, GradeDirName),
        !:Specs = [],
        check_stdlib_is_installed(Globals, GradeDirName, !Specs, !IO),
        list.foldl2(check_library_is_installed(Globals, GradeDirName), Libs,
            !Specs, !IO)
    ;
        LibgradeCheck = no,
        !:Specs = []
    ).

:- pred check_stdlib_is_installed(globals::in, string::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_stdlib_is_installed(Globals, GradeDirName, !Specs, !IO) :-
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
            io.close_input(StdLibCheckFileStream, !IO)
        ;
            StdLibCheckFileResult = error(_),
            % XXX It would be better for our *caller* to print this kind of
            % message, since it may know a more appropriate target stream
            % than stderr.
            io.progname_base("mercury_compile", ProgName, !IO),
            Pieces = [fixed(ProgName), suffix(":"), words("error:"),
                words("the Mercury standard library cannot be found"),
                words("in grade"), quote(GradeDirName), suffix("."), nl],
            Spec = simplest_no_context_spec($pred, severity_error,
                phase_check_libs, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        MaybeStdLibDir = no
    ).

:- pred check_library_is_installed(globals::in, string::in, string::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_library_is_installed(Globals, GradeDirName, LibName, !Specs, !IO) :-
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
        io.progname_base("mercury_compile", ProgName, !IO),
        Pieces = [fixed(ProgName), suffix(":"), words("error:"),
            words("the library"), quote(LibName), words("cannot be found"),
            words("in grade"), quote(GradeDirName), suffix("."), nl],
        Spec = simplest_no_context_spec($pred, severity_error,
            phase_check_libs, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%
:- end_module libs.check_libgrades.
%---------------------------------------------------------------------------%
