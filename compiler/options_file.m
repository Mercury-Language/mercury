%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: options_file.m.
% Main author: stayl.
%
% Code to deal with options for `mmc --make', including code to parse
% Mercury.options files.
%
%---------------------------------------------------------------------------%

:- module make.options_file.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type options_variables.

:- func options_variables_init(environment_var_map) = options_variables.

    % read_options_files_named_in_options_file_option(OptionSearchDirs,
    %   OptionsFiles, Variables, Specs, UndefSpecs, !IO):
    %
    % Given OptionSearchDirs, the value of the options_search_directories
    % option, and OptionsFiles, the value of the options_files option,
    % look up and read all the files named in OptionsFiles in OptionSearchDirs.
    % Construct a database of variable name/value pairs from the make variable
    % assignments in those files (and the other files they may include, either
    % directly or indirectly) and return it as Variables.
    %
    % We return two lists of error specs. The second should be printed
    % only if the option warn_undefined_options_variables is set, while
    % the first list should be printed unconditionally.
    %
:- pred read_options_files_named_in_options_file_option(list(string)::in,
    list(string)::in, options_variables::out,
    list(error_spec)::out, list(error_spec)::out, io::di, io::uo) is det.

    % read_named_options_file(OptionsPathName, !Variables,
    %   Specs, UndefSpecs, !IO) :-
    %
    % Read the given options file, without searching
    % --options-search-directories, updating the database of make variable
    % name/value pairs. This is used to read the configuration file.
    %
    % We return two lists of error specs. The second should be printed
    % only if the option warn_undefined_options_variables is set, while
    % the first list should be printed unconditionally.
    %
:- pred read_named_options_file(file_name::in,
    options_variables::in, options_variables::out,
    list(error_spec)::out, list(error_spec)::out, io::di, io::uo) is det.

    % read_args_file(OptionsFile, MaybeMCFlags, Specs, UndefSpecs, !IO):
    %
    % Read a single options file. No searching will be done. The result is
    % the value of the variable MCFLAGS obtained from the file, ignoring
    % settings in the environment. This is used to pass arguments to child
    % mmc processes without exceeding command line limits on crappy operating
    % systems.
    %
    % This is not quite the same as @file syntax as the environment is ignored.
    %
    % We return two lists of error specs. The second should be printed
    % only if the option warn_undefined_options_variables is set, while
    % the first list should be printed unconditionally.
    % XXX But see the comments near the only call to this predicate
    % in mercury_compile_main.m.
    %
:- pred read_args_file(file_name::in,
    maybe(list(string))::out, list(error_spec)::out, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Look up $(MAIN_TARGET).
    %
:- pred lookup_main_target(options_variables::in,
    maybe1(list(string))::out) is det.

    % Look up $(MERCURY_STDLIB_DIR).
    %
:- pred lookup_mercury_stdlib_dir(options_variables::in,
    maybe1(list(string))::out) is det.

    % Look up the DEFAULT_MCFLAGS variable.
    %
:- pred lookup_default_options(options_variables::in,
    maybe1(list(string))::out) is det.

    % Look up all the non-module specific options.
    %
:- pred lookup_mmc_options(options_variables::in,
    maybe1(list(string))::out) is det.

    % Same as lookup_mmc_options, but also adds the module-specific
    % (MCFLAGS-module) options.
    %
:- pred lookup_mmc_module_options(options_variables::in, module_name::in,
    maybe1(list(string))::out) is det.

%---------------------------------------------------------------------------%

    % dump_options_file(ErrorStream, FileName, Vars, !IO):
    %
    % Write out the given database given by Vars to a file named FileName,
    % for testing the functionality of code that builds such databases.
    % Report any inability to open FileName to ErrorStream.
    %
:- pred dump_options_file(io.text_output_stream::in, file_name::in,
    options_variables::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.find_module.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module io.environment.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

:- type options_variable == string.

:- type options_file_error
    --->    options_file_error(string).

:- type found_options_file_error
    --->    found_options_file_error.

:- type options_variable_value
    --->    options_variable_value(
                list(char),     % The variable's value.
                list(string),   % The variable's value split into words.
                variable_source
            ).

:- type variable_source
    --->    options_file
    ;       command_line
    ;       environment.

:- type options_variables
    --->    options_variables(
                ov_opts     :: map(options_variable, options_variable_value),
                ov_env      :: map(options_variable, string)
            ).

options_variables_init(EnvVarMap) = Variables :-
    map.init(OptsMap),
    Variables = options_variables(OptsMap, EnvVarMap).

%---------------------------------------------------------------------------%

read_options_files_named_in_options_file_option(OptionSearchDirs, OptionsFiles,
        Variables, Specs, UndefSpecs, !IO) :-
    io.environment.get_environment_var_map(EnvVarMap, !IO),
    Variables0 = options_variables_init(EnvVarMap),
    list.foldl5(
        read_options_file_set_params(OptionSearchDirs), OptionsFiles,
        Variables0, Variables,
        [], IOSpecs, [], ParseSpecs, [], UndefSpecs, !IO),
    Specs = IOSpecs ++ ParseSpecs.

:- pred read_options_file_set_params(list(string)::in, string::in,
    options_variables::in, options_variables::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_options_file_set_params(OptionSearchDirs, OptionsFile,
        !Variables, !IOSpecs, !ParseSpecs, !UndefSpecs, !IO) :-
    MaybeDirName = no,
    ( if OptionsFile = "Mercury.options" then
        MaybeSearch = no_search,
        IsOptionsFileOptional = options_file_need_not_exist
    else
        MaybeSearch = search(OptionSearchDirs),
        IsOptionsFileOptional = options_file_must_exist
    ),
    SearchInfo = search_info(MaybeDirName, MaybeSearch),
    read_options_file_params(SearchInfo, pre_stack_base, IsOptionsFileOptional,
        OptionsFile, !Variables, !IOSpecs, !ParseSpecs, !UndefSpecs, !IO).

%---------------------%

read_named_options_file(OptionsPathName, !Variables, Specs, UndefSpecs, !IO) :-
    SearchInfo = search_info(no, no_search),
    read_options_file_params(SearchInfo, pre_stack_base,
        options_file_must_exist, OptionsPathName, !Variables,
        [], IOSpecs, [], ParseSpecs, [], UndefSpecs, !IO),
    Specs = IOSpecs ++ ParseSpecs.

%---------------------%

read_args_file(OptionsFile, MaybeMCFlags, Specs, UndefSpecs, !IO) :-
    io.environment.get_environment_var_map(EnvVarMap, !IO),
    Variables0 = options_variables_init(EnvVarMap),
    read_named_options_file(OptionsFile, Variables0, Variables,
        Specs0, UndefSpecs, !IO),
    % Ignore settings in the environment -- the parent mmc process
    % will have included those in the file.
    NoEnvVariables = Variables ^ ov_env := map.init,
    lookup_variable_words(NoEnvVariables, "MCFLAGS", FlagsResult),
    (
        FlagsResult = var_result_set(MCFlags),
        MaybeMCFlags = yes(MCFlags),
        Specs = Specs0
    ;
        FlagsResult = var_result_unset,
        MaybeMCFlags = no,
        ( if Specs0 = [], UndefSpecs = [] then
            Pieces = [words("mercury_compile: internal error:"),
                words("arguments file does not set MCFLAGS."), nl],
            Spec = simplest_no_context_spec($pred, severity_error,
                phase_read_files, Pieces),
            Specs = [Spec | Specs0]
        else
            % Any of the errors in Specs or UndefSpecs could be the reason
            % why we missed a setting of MCFLAGS, so we don't know that
            % an internal error exists.
            Specs = Specs0
        )
    ;
        FlagsResult = var_result_error(OoMErrorSpecs),
        MaybeMCFlags = no,
        VarResultSpecs = one_or_more_to_list(OoMErrorSpecs),
        Specs = VarResultSpecs ++ Specs0
    ).

%---------------------------------------------------------------------------%

:- type search
    --->    search(list(dir_name))
            % This should be the value of options_search_directories.
    ;       no_search.

:- type search_info
    --->    search_info(
                maybe(dir_name),
                search
            ).

:- type is_options_file_optional
    --->    options_file_need_not_exist
    ;       options_file_must_exist.

    % The inclusion stack records, for the options file being processed,
    % which other options files, if any, contained the include directives
    % that lead to it being read. We use it to detect circular inclusions.
:- type incl_stack
    --->    incl_stack_base(
                % The file named here is either read automatically by
                % the compiler (e.g. Mercury.options) or its reading
                % was requested by the user via an --options-file
                % compiler option.
                file_name
            )
    ;       incl_stack_nested(
                % We read the file named here in response to an "include"
                % directive.
                file_name,

                % The context of that include directive.
                term_context,

                % The "provenance" of the file that contains that include
                % directive.
                incl_stack
            ).

    % The pre_incl_stack is a version of the incl_stack *before* file_util.m
    % finds the full pathname of a possibly-searched-for options file for us.
:- type pre_incl_stack
    --->    pre_stack_base
    ;       pre_stack_nested(term_context, incl_stack).

:- pred read_options_file_params(search_info::in,
    pre_incl_stack::in, is_options_file_optional::in,
    string::in, options_variables::in, options_variables::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_options_file_params(SearchInfo, PreStack0, IsOptionsFileOptional,
        OptionsPathName, !Variables,
        !IOSpecs, !ParseSpecs, !UndefSpecs, !IO) :-
    % Note that any debugging output we generate goes to stderr.
    % Reading the options file is an activity that is not specific
    % to any module, so it cannot go to a module-specific debug output file.
    ( if OptionsPathName = "-" then
        check_include_for_infinite_recursion(PreStack0, "-", CheckResult),
        (
            CheckResult = include_ok(InclStack0),
            % Read from standard input.
            trace [compiletime(flag("options_file_debug")), io(!TIO)] (
                io.stderr_stream(DebugStream, !TIO),
                io.write_string(DebugStream,
                    "Reading options file from stdin... ", !TIO)
            ),
            SearchInfo = search_info(_MaybeDirName, Search),
            SubSearchInfo = search_info(yes(dir.this_directory), Search),
            read_options_lines(SubSearchInfo, InclStack0,
                io.stdin_stream, "stdin", 1, !Variables,
                !IOSpecs, !ParseSpecs, !UndefSpecs, !IO),
            trace [compiletime(flag("options_file_debug")), io(!TIO)] (
                io.stderr_stream(DebugStream, !TIO),
                io.format(DebugStream, "done.\n", [], !TIO)
            ),
            trace [compiletime(flag("options_file_debug_stdin")), io(!TIO)] (
                io.stderr_stream(DebugStream, !TIO),
                dump_options_file(DebugStream, "DUMP_OPTIONS_FILE",
                    !.Variables, !TIO)
            )
        ;
            CheckResult = include_error(CheckSpec),
            !:ParseSpecs = [CheckSpec | !.ParseSpecs]
        )
    else
        trace [compiletime(flag("options_file_debug")), io(!TIO)] (
            io.stderr_stream(DebugStream, !TIO),
            io.format(DebugStream, "Searching for options file %s... ",
                [s(OptionsPathName)], !TIO)
        ),
        SearchInfo = search_info(MaybeDirName, Search),
        (
            Search = search(SearchDirs)
        ;
            Search = no_search,
            SearchDirs = [dir.this_directory]
        ),
        % XXX There are four distinct paths through this if-then-else.
        % The first, second and fourth of these set FileToFind to a file name
        % that has no dir name component (the first and second because
        % dir.split_name has removed the dir name component, the fourth
        % because dir.split_name has not found any split name component.
        % So why in hell does the third path set FileToFind to a file name
        % that is *known* to have a directory name component?
        % In the original form of this code, the OptionsPathName variable
        % had its original name, OptionsFile0, so it *could* have been
        % a typo, but if so, why hasn't it been caught?
        ( if dir.split_name(OptionsPathName, OptionsDir, OptionsFile) then
            ( if dir.path_name_is_absolute(OptionsDir) then
                % In this case, the "search" below won't really be a search,
                % but since absolute path names occur rarely, restructuring
                % this code to avoid that "search" is not worthwhile.
                FileToFind = OptionsFile,
                Dirs = [OptionsDir]
            else
                (
                    MaybeDirName = yes(DirName),
                    Dirs = [DirName/OptionsDir | SearchDirs],
                    FileToFind = OptionsFile
                ;
                    MaybeDirName = no,
                    Dirs = SearchDirs,
                    FileToFind = OptionsPathName
                )
            )
        else
            Dirs = SearchDirs,
            FileToFind = OptionsPathName
        ),
        search_for_file_returning_dir_and_stream(Dirs, FileToFind,
            MaybeDirAndStream, !IO),
        (
            MaybeDirAndStream =
                ok(path_name_and_stream(FoundDir, FoundStream)),
            trace [compiletime(flag("options_file_debug")), io(!TIO)] (
                io.stderr_stream(DebugStream, !TIO),
                io.format(DebugStream, "done.\n", [], !TIO)
            ),
            check_include_for_infinite_recursion(PreStack0,
                FoundDir / FileToFind, CheckResult),
            (
                CheckResult = include_ok(InclStack0),
                trace [compiletime(flag("options_file_debug")), io(!TIO)] (
                    io.stderr_stream(DebugStream, !TIO),
                    io.format(DebugStream, "Reading options file %s... ",
                        [s(FoundDir/FileToFind)], !TIO)
                ),

                % XXX Instead of setting and unsetting the input stream,
                % we should simply pass FoundStream to read_options_lines.
                % However, when I (zs) tried that, I quickly found that
                % the call tree of read_options_lines includes many predicates
                % for which it is not at all clear whether they *intend*
                % to read from a current standard input that originates as
                % FoundStream, or they just *happen* to do so.

                SubSearchInfo = search_info(yes(FoundDir), Search),
                read_options_lines(SubSearchInfo, InclStack0,
                    FoundStream, FileToFind, 1, !Variables,
                    !IOSpecs, !ParseSpecs, !UndefSpecs, !IO),
                trace [compiletime(flag("options_file_debug")), io(!TIO)] (
                    io.stderr_stream(DebugStream, !TIO),
                    io.format(DebugStream, "done.\n", [], !TIO)
                )
            ;
                CheckResult = include_error(CheckSpec),
                !:ParseSpecs = [CheckSpec | !.ParseSpecs]
            ),
            io.close_input(FoundStream, !IO)
        ;
            MaybeDirAndStream = error(Error),
            trace [compiletime(flag("options_file_debug")), io(!TIO)] (
                io.stderr_stream(DebugStream, !TIO),
                io.format(DebugStream, "unsuccessful.\n", [], !TIO)
            ),
            (
                IsOptionsFileOptional = options_file_must_exist,
                ( if Dirs = [SingleDir] then
                    ( if SingleDir = dir.this_directory then
                        ErrorFile = FileToFind
                    else
                        ErrorFile = dir.make_path_name(SingleDir, FileToFind)
                    )
                else
                    ErrorFile = FileToFind
                ),
                (
                    PreStack0 = pre_stack_base,
                    MaybeContext = no
                ;
                    PreStack0 = pre_stack_nested(Context, _),
                    MaybeContext = yes(Context)
                ),
                Pieces = [words("Cannot open options file"),
                    quote(ErrorFile), suffix(":"),
                    words(Error), suffix("."), nl],
                Spec = error_spec($pred, severity_error, phase_read_files,
                    [error_msg(MaybeContext, always_treat_as_first, 0,
                        [always(Pieces)])]),
                !:IOSpecs = [Spec | !.IOSpecs]
            ;
                IsOptionsFileOptional = options_file_need_not_exist
            )
        )
    ).

%---------------------%

:- type include_check_result
    --->    include_ok(incl_stack)
    ;       include_error(error_spec).

:- pred check_include_for_infinite_recursion(pre_incl_stack::in,
    file_name::in, include_check_result::out) is det.

check_include_for_infinite_recursion(PreStack0, PathName, Result) :-
    (
        PreStack0 = pre_stack_base,
        InclStack = incl_stack_base(PathName),
        Result = include_ok(InclStack)
    ;
        PreStack0 = pre_stack_nested(Context, InclStack0),
        ( if
            pathname_occurs_in_incl_stack(InclStack0, PathName, Context, Spec)
        then
            Result = include_error(Spec)
        else
            InclStack = incl_stack_nested(PathName, Context, InclStack0),
            Result = include_ok(InclStack)
        )
    ).

:- pred pathname_occurs_in_incl_stack(incl_stack::in, file_name::in,
    term_context::in, error_spec::out) is semidet.

pathname_occurs_in_incl_stack(InclStack0, PathName, Context, Spec) :-
    (
        InclStack0 = incl_stack_base(StackPathName0),
        ( if PathName = StackPathName0 then
            Pieces = [words("Error: options file"), quote(PathName),
                words("includes itself."), nl],
            Spec = simplest_spec($pred, severity_error, phase_read_files,
                Context, Pieces)
        else
            fail
        )
    ;
        InclStack0 = incl_stack_nested(StackPathName0, Context0, InclStack1),
        ( if PathName = StackPathName0 then
            Pieces = [words("Error: options file"), quote(PathName),
                words("includes itself."), nl],
            Spec = simplest_spec($pred, severity_error, phase_read_files,
                Context, Pieces)
        else
            ( if
                pathname_occurs_in_incl_stack_2(InclStack1, PathName,
                    [StackPathName0 - Context0], TopDownIncludes)
            then
                TopPathName - TopContext = list.det_head(TopDownIncludes),
                MainPieces = [words("Error: options file"), quote(TopPathName),
                    words("indirectly includes itself through"),
                    words("the following chain of include directives."), nl],
                MainMsg = simplest_msg(TopContext, MainPieces),
                InclMsgs = list.map(include_context_msg, TopDownIncludes),
                LastMsg = include_context_msg(PathName - Context),
                Spec = error_spec($pred, severity_error, phase_read_files,
                    [MainMsg | InclMsgs] ++ [LastMsg])
            else
                fail
            )
        )
    ).

:- pred pathname_occurs_in_incl_stack_2(incl_stack::in, file_name::in,
    assoc_list(file_name, term_context)::in,
    assoc_list(file_name, term_context)::out) is semidet.

pathname_occurs_in_incl_stack_2(InclStack0, PathName, !TopDownIncludes) :-
    (
        InclStack0 = incl_stack_base(StackPathName0),
        PathName = StackPathName0
    ;
        InclStack0 = incl_stack_nested(StackPathName0, Context0, InclStack1),
        !:TopDownIncludes = [StackPathName0 - Context0 | !.TopDownIncludes],
        ( if PathName = StackPathName0 then
            true
        else
            pathname_occurs_in_incl_stack_2(InclStack1, PathName,
                !TopDownIncludes)
        )
    ).

:- func include_context_msg(pair(file_name, term_context)) = error_msg.

include_context_msg(FileName - Context) = Msg :-
    Pieces = [words("The include directive for"), quote(FileName),
        words("here."), nl],
    Msg = simplest_msg(Context, Pieces).

%---------------------------------------------------------------------------%

:- type maybe_is_first
    --->    is_not_first
    ;       is_first.

:- type parse_result(T)
    --->    pr_ok(T)
    ;       pr_eof
    ;       pr_error(error_spec).

%---------------------------------------------------------------------------%

:- pred read_options_lines(search_info::in, incl_stack::in,
    io.text_input_stream::in, file_name::in, int::in,
    options_variables::in, options_variables::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

read_options_lines(SearchInfo, InclStack0, InStream, FileName, LineNumber0,
        !Variables, !IOSpecs, !ParseSpecs, !UndefSpecs, !IO) :-
    read_options_line(InStream, FileName, LineNumber0, LineNumber1,
        LineResult, !IO),
    (
        LineResult = pr_ok(LineChars0),
        (
            LineChars0 = []
        ;
            LineChars0 = [_ | _],
            parse_options_line(FileName, LineNumber0, LineChars0,
                MaybeOptionsFileLine),
            (
                MaybeOptionsFileLine = ofl_ok(OptionsFileLine),
                (
                    OptionsFileLine = ofl_var_defn(SetOrAdd, VarName, Value),
                    update_variable(FileName, LineNumber1,
                        SetOrAdd, VarName, Value, !Variables,
                        !ParseSpecs, !UndefSpecs, !IO)
                ;
                    OptionsFileLine = ofl_include(IsOptionsFileOptional,
                        IncludedFilesChars0),
                    expand_any_var_references(!.Variables,
                        FileName, LineNumber0,
                        IncludedFilesChars0, IncludedFilesChars,
                        !ParseSpecs, !UndefSpecs, !IO),
                    MaybeIncludedFileNames =
                        split_into_words(IncludedFilesChars),
                    (
                        MaybeIncludedFileNames = ok(IncludedFileNames),
                        Context = term_context.context(FileName, LineNumber0),
                        PreStack1 = pre_stack_nested(Context, InclStack0),
                        list.foldl5(
                            read_options_file_params(SearchInfo,
                                PreStack1, IsOptionsFileOptional),
                            IncludedFileNames, !Variables,
                            !IOSpecs, !ParseSpecs, !UndefSpecs, !IO)
                    ;
                        MaybeIncludedFileNames = error(IncludeFileNamesError),
                        Spec = report_split_error(FileName, LineNumber0,
                            IncludeFileNamesError),
                        !:ParseSpecs = [Spec | !.ParseSpecs]
                    )
                )
            ;
                MaybeOptionsFileLine = ofl_error(ParseSpec),
                !:ParseSpecs = [ParseSpec | !.ParseSpecs]
            )
        ),
        LineNumber2 = LineNumber1 + 1,
        read_options_lines(SearchInfo, InclStack0, InStream,
            FileName, LineNumber2, !Variables,
            !IOSpecs, !ParseSpecs, !UndefSpecs, !IO)
    ;
        LineResult = pr_error(Spec),
        !:IOSpecs = [Spec | !.IOSpecs]
    ;
        LineResult = pr_eof
    ).

:- pred read_options_line(io.text_input_stream::in, file_name::in,
    int::in, int::out, parse_result(list(char))::out, io::di, io::uo) is det.

read_options_line(InStream, FileName, !LineNumber, Result, !IO) :-
    io.ignore_whitespace(InStream, SpaceResult, !IO),
    (
        SpaceResult = ok,
        read_options_line_loop(InStream, FileName, !LineNumber, [],
            Result, !IO)
    ;
        SpaceResult = eof,
        Result = pr_eof
    ;
        SpaceResult = error(Error),
        Spec = io_error_to_parse_error(FileName, !.LineNumber, Error),
        Result = pr_error(Spec)
    ).

    % If we find an I/O error while reading the line, return pr_error(...).
    % Otherwise, if there is anything on the line, return pr_ok(...)
    % wrapped around the non-comment characters.
    % If there is nothing on the line, return pr_eof.
    %
:- pred read_options_line_loop(io.text_input_stream::in, file_name::in,
    int::in, int::out, list(char)::in, parse_result(list(char))::out,
    io::di, io::uo) is det.

read_options_line_loop(InStream, FileName, !LineNumber, !.RevChars,
        Result, !IO) :-
    io.read_char_unboxed(InStream, CharResult, Char, !IO),
    (
        CharResult = ok,
        ( if Char = '#' then
            list.reverse(!.RevChars, Chars),
            skip_comment_line(InStream, SkipResult, !IO),
            (
                ( SkipResult = ok
                ; SkipResult = eof
                ),
                % Even if the skip found an eof, return the line so far
                % before the hash sign (which may be empty). The attempt
                % to read in the next line will find eof again, this time
                % *without* a hash sign, which will cause us to return eof
                % as well.
                Result = pr_ok(Chars)
            ;
                SkipResult = error(Error),
                Spec = io_error_to_parse_error(FileName, !.LineNumber, Error),
                Result = pr_error(Spec)
            )
        else if Char = ('\\') then
            io.read_char_unboxed(InStream, CharResult2, Char2, !IO),
            (
                CharResult2 = ok,
                ( if Char2 = '\n' then
                    !:LineNumber = !.LineNumber + 1,
                    !:RevChars = [' ' | !.RevChars]
                else
                    !:RevChars = [Char2, Char | !.RevChars]
                ),
                read_options_line_loop(InStream, FileName, !LineNumber,
                    !.RevChars, Result, !IO)
            ;
                CharResult2 = eof,
                Context = term_context.context(FileName, !.LineNumber),
                Pieces = [words("Error: attempt to escape end-of-file."), nl],
                Spec = simplest_spec($pred, severity_error, phase_read_files,
                    Context, Pieces),
                Result = pr_error(Spec)
            ;
                CharResult2 = error(Error),
                Spec = io_error_to_parse_error(FileName, !.LineNumber, Error),
                Result = pr_error(Spec)
            )
        else if Char = '\n' then
            % Our caller will increment the line number.
            list.reverse(!.RevChars, Chars),
            Result = pr_ok(Chars)
        else
            !:RevChars = [Char | !.RevChars],
            read_options_line_loop(InStream, FileName, !LineNumber,
                !.RevChars, Result, !IO)
        )
    ;
        CharResult = eof,
        (
            !.RevChars = [_ | _],
            list.reverse(!.RevChars, Chars),
            Result = pr_ok(Chars)
        ;
            !.RevChars = [],
            Result = pr_eof
        )
    ;
        CharResult = error(Error),
        Spec = io_error_to_parse_error(FileName, !.LineNumber, Error),
        Result = pr_error(Spec)
    ).

:- func io_error_to_parse_error(file_name, int, io.error) = error_spec.

io_error_to_parse_error(FileName, LineNumber, Error) = Spec :-
    Context = term_context.context(FileName, LineNumber),
    Msg = io.error_message(Error),
    Pieces = [words("I/O error:"), words(Msg), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_read_files,
        Context, Pieces).

:- func report_split_error(file_name, int, string) = error_spec.

report_split_error(FileName, LineNumber, Msg) = Spec :-
    Context = term_context.context_init(FileName, LineNumber),
    Pieces = [words("Error:"), words(Msg), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_read_files,
        Context, Pieces).

%---------------------------------------------------------------------------%

:- pred skip_comment_line(io.text_input_stream::in, io.result::out,
    io::di, io::uo) is det.

skip_comment_line(InStream, Result, !IO) :-
    io.read_char_unboxed(InStream, CharResult, Char, !IO),
    (
        CharResult = ok,
        ( if Char = '\n' then
            Result = ok
        else
            skip_comment_line(InStream, Result, !IO)
        )
    ;
        CharResult = eof,
        Result = eof
    ;
        CharResult = error(Error),
        Result = error(Error)
    ).

%---------------------------------------------------------------------------%

:- type maybe_options_file_line
    --->    ofl_ok(options_file_line)
    ;       ofl_error(error_spec).

:- type options_file_line
    --->    ofl_var_defn(
                vd_set_or_add       :: set_or_add,
                vd_var              :: options_variable,
                vd_value            :: list(char)
            )
    ;       ofl_include(
                i_exist             :: is_options_file_optional,
                i_incl_file_name    :: list(char)
            ).

:- type set_or_add
    --->    soa_set
    ;       soa_add.

:- pred parse_options_line(file_name::in, int::in,
    list(char)::in, maybe_options_file_line::out) is det.

parse_options_line(FileName, LineNumber, Line0, MaybeOptionsFileLine) :-
    ( if
        ( if Line0 = [('-') | Line1] then
            IsOptionsFileOptional = options_file_need_not_exist,
            Line2 = Line1
        else
            IsOptionsFileOptional = options_file_must_exist,
            Line2 = Line0
        ),
        list.append(string.to_char_list("include"), AfterInclude0, Line2)
    then
        list.drop_while(char.is_whitespace, AfterInclude0, AfterInclude),
        MaybeOptionsFileLine = ofl_ok(
            ofl_include(IsOptionsFileOptional, AfterInclude))
    else
        parse_variable_name(FileName, LineNumber, Line0, Line1,
            MaybeVarName),
        (
            MaybeVarName = ovos_spec(Spec),
            MaybeOptionsFileLine = ofl_error(Spec)
        ;
            MaybeVarName = ovos_var_name(VarName),
            list.drop_while(char.is_whitespace, Line1, Line2),
            ( if
                (
                    ( Line2 = [('=') | Line3]
                    ; Line2 = [(':'), ('=') | Line3]
                    ),
                    SetOrAdd = soa_set
                ;
                    Line2 = [('+'), ('=') | Line3],
                    SetOrAdd = soa_add
                )
            then
                list.drop_while(char.is_whitespace, Line3, VarValue),
                MaybeOptionsFileLine = ofl_ok(
                    ofl_var_defn(SetOrAdd, VarName, VarValue))
            else
                Context = term_context.context(FileName, LineNumber),
                Pieces = [words("expected"), quote("="), suffix(","),
                    quote(":="), words("or"), quote("+="),
                    words("after"), quote(VarName), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error, phase_read_files,
                    Context, Pieces),
                MaybeOptionsFileLine = ofl_error(Spec)
            )
        )
    ).

:- type opt_var_or_spec
    --->    ovos_var_name(options_variable)
    ;       ovos_spec(error_spec).

:- pred parse_variable_name(file_name::in, int::in,
    list(char)::in, list(char)::out, opt_var_or_spec::out) is det.

parse_variable_name(FileName, LineNumber, Chars0, Chars, MaybeVarName) :-
    do_parse_variable_name(Chars0, Chars, is_first, [], RevVarNameChars),
    string.from_rev_char_list(RevVarNameChars, VarName),
    ( if VarName = "" then
        list.take_while(isnt(char.is_whitespace), Chars, FirstWordChars),
        Pieces = [words("expected variable name before"),
            quote(string.from_char_list(FirstWordChars)), suffix("."), nl],
        Context = term_context.context(FileName, LineNumber),
        Spec = simplest_spec($pred, severity_error, phase_read_files,
            Context, Pieces),
        MaybeVarName = ovos_spec(Spec)
    else
        MaybeVarName = ovos_var_name(VarName)
    ).

:- pred do_parse_variable_name(list(char)::in, list(char)::out,
    maybe_is_first::in, list(char)::in, list(char)::out) is det.

do_parse_variable_name([], [], _, !RevVarNameChars).
do_parse_variable_name([Char | Chars0], Chars, IsFirst, !RevVarNameChars) :-
    ( if
        not char.is_whitespace(Char),
        (
            IsFirst = is_first,
            char.is_alpha(Char)
        ;
            IsFirst = is_not_first,
            ( char.is_alnum_or_underscore(Char)
            ; Char = ('-')
            ; Char = ('.')
            )
        )
    then
        !:RevVarNameChars = [Char | !.RevVarNameChars],
        do_parse_variable_name(Chars0, Chars, is_not_first, !RevVarNameChars)
    else
        Chars = [Char | Chars0]
    ).

%---------------------------------------------------------------------------%

:- func split_into_words(list(char)) = maybe_error(list(string)).

split_into_words(Chars) = Result :-
    split_into_words_loop(Chars, [], RevWords, MaybeError),
    list.reverse(RevWords, Words),
    (
        MaybeError = no,
        Result = ok(Words)
    ;
        MaybeError = yes(Msg),
        Result = error(Msg)
    ).

:- pred split_into_words_loop(list(char)::in,
    list(string)::in, list(string)::out, maybe(string)::out) is det.

split_into_words_loop(Chars0, !RevWords, MaybeError) :-
    list.drop_while(char.is_whitespace, Chars0, Chars1),
    (
        Chars1 = [],
        MaybeError = no
    ;
        Chars1 = [_ | _],
        get_word(Chars1, Chars, Word, MaybeError0),
        (
            MaybeError0 = no,
            !:RevWords = [Word | !.RevWords],
            split_into_words_loop(Chars, !RevWords, MaybeError)
        ;
            MaybeError0 = yes(_),
            MaybeError = MaybeError0
        )
    ).

%---------------------%

    % get_word(Chars0, Chars, Word, MaybeError):
    %
    % Read one word from Chars0, returning the remaining characters in Chars
    % and the word itself in Word, if MaybeError = no. If MaybeError is
    % yes(Error), then Error will describe the error, abd the none of
    % the other return values will be meaningful.
    %
    % A word is defined as a sequence of either
    % - non-whitespace characters,
    % - characters escaped with a backslash (which may be whitespace chars), or
    % - strings starting and ending with unescaped double quotes (which may
    %   also contain whitespace chars).
    %
:- pred get_word(list(char)::in, list(char)::out,
    string::out, maybe(string)::out) is det.

get_word(Chars0, Chars, Word, MaybeError) :-
    get_word_acc(Chars0, Chars, [], RevWord, MaybeError),
    Word = string.from_rev_char_list(RevWord).

:- pred get_word_acc(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out, maybe(string)::out) is det.

get_word_acc([], [], RevWord, RevWord, no).
get_word_acc([Char | Chars0], Chars, RevWord0, RevWord, MaybeError) :-
    ( if char.is_whitespace(Char) then
        Chars = Chars0,
        RevWord = RevWord0,
        MaybeError = no
    else if Char = '"' then
        get_string_acc(Chars0, Chars1, [], RevStringChars, MaybeError0),
        (
            MaybeError0 = no,
            get_word_acc(Chars1, Chars, RevStringChars ++ RevWord0, RevWord,
                MaybeError)
        ;
            MaybeError0 = yes(_),
            Chars = Chars0,
            RevWord = RevWord0,
            MaybeError = MaybeError0
        )
    else if Char = ('\\') then
        (
            Chars0 = [],
            Chars = [],
            RevWord = [Char | RevWord0],
            MaybeError = no
        ;
            Chars0 = [Char2 | Chars1],
            ( if
                ( Char2 = '"'
                ; Char2 = ('\\')
                )
            then
                RevWord1 = [Char2 | RevWord0]
            else
                RevWord1 = [Char2, Char | RevWord0]
            ),
            get_word_acc(Chars1, Chars, RevWord1, RevWord, MaybeError)
        )
    else
        get_word_acc(Chars0, Chars, [Char | RevWord0], RevWord, MaybeError)
    ).

    % get_string_acc(Chars0, Chars, RevString0, RevString, MaybeError):
    %
    % Read the part of a double-quoted string from Chars0 that occurs
    % after the initial double quote, returning the remaining characters
    % in Chars and adding the characters of the string itself in reverse
    % to RevString0 yielding RevString, if MaybeError = no. If MaybeError is
    % yes(Error), then Error will describe the error, abd the none of
    % the other return values will be meaningful.
    %
:- pred get_string_acc(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out, maybe(string)::out) is det.

get_string_acc([], [], RevString0, RevString0, MaybeError) :-
    MaybeError = yes("unterminated string").
get_string_acc([Char | Chars0], Chars, RevString0, RevString, MaybeError) :-
    ( if Char = '"' then
        Chars = Chars0,
        RevString = RevString0,
        MaybeError = no
    else if Char = ('\\') then
        (
            Chars0 = [Char2 | Chars1],
            ( if Char2 = '"' then
                RevString1 = [Char2 | RevString0]
            else
                RevString1 = [Char2, Char | RevString0]
            ),
            get_string_acc(Chars1, Chars, RevString1, RevString, MaybeError)
        ;
            Chars0 = [],
            Chars = Chars0,
            RevString = RevString0,
            MaybeError = yes("unterminated string")
        )
    else
        get_string_acc(Chars0, Chars, [Char | RevString0], RevString,
            MaybeError)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred update_variable(file_name::in, int::in,
    set_or_add::in, options_variable::in, list(char)::in,
    options_variables::in, options_variables::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

update_variable(FileName, LineNumber, SetOrAdd, VarName, NewValue0,
        !Variables, !ParseSpecs, !UndefSpecs, !IO) :-
    expand_any_var_references(!.Variables, FileName, LineNumber,
        NewValue0, NewValue1, !ParseSpecs, !UndefSpecs, !IO),
    MaybeWords1 = split_into_words(NewValue1),
    (
        MaybeWords1 = ok(Words1),
        !.Variables = options_variables(OptsMap0, EnvMap),
        ( if map.search(EnvMap, VarName, EnvValue) then
            Value = string.to_char_list(EnvValue),
            MaybeWords = split_into_words(Value),
            (
                MaybeWords = ok(Words),
                EnvValueChars = string.to_char_list(EnvValue),
                Entry = options_variable_value(EnvValueChars, Words,
                    environment),
                map.set(VarName, Entry, OptsMap0, OptsMap),
                !:Variables = options_variables(OptsMap, EnvMap)
            ;
                MaybeWords = error(WordsError),
                Spec = report_split_error(FileName, LineNumber, WordsError),
                !:ParseSpecs = [Spec | !.ParseSpecs]
            )
        else
            ( if map.search(!.Variables ^ ov_opts, VarName, OldEntry) then
                OldEntry = options_variable_value(OldValue, OldWords, Source),
                (
                    Source = environment
                ;
                    Source = command_line
                ;
                    Source = options_file,
                    (
                        SetOrAdd = soa_set,
                        NewValue = NewValue1,
                        Words = Words1
                    ;
                        SetOrAdd = soa_add,
                        NewValue = OldValue ++ [' ' |  NewValue1],
                        Words = OldWords ++ Words1
                    ),
                    Entry = options_variable_value(NewValue, Words,
                        options_file),
                    map.det_update(VarName, Entry, OptsMap0, OptsMap),
                    !:Variables = options_variables(OptsMap, EnvMap)
                )
            else
                Entry = options_variable_value(NewValue1, Words1,
                    options_file),
                map.det_insert(VarName, Entry, OptsMap0, OptsMap),
                !:Variables = options_variables(OptsMap, EnvMap)
            )
        )
    ;
        MaybeWords1 = error(WordsError1),
        Spec = report_split_error(FileName, LineNumber, WordsError1),
        !:ParseSpecs = [Spec | !.ParseSpecs]
    ).

%---------------------------------------------------------------------------%

:- pred expand_any_var_references(options_variables::in,
    file_name::in, int::in, list(char)::in, list(char)::out,
    list(error_spec)::in, list(error_spec)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

expand_any_var_references(Variables, FileName, LineNumber, Chars0, Chars,
        !ParseSpecs, !UndefSpecs, !IO) :-
    expand_any_var_references_loop(Variables, FileName, LineNumber,
        Chars0, [], RevChars, !ParseSpecs, set.init, UndefVarNames, !IO),
    list.reverse(RevChars, Chars),
    report_any_undefined_variables(FileName, LineNumber, UndefVarNames,
        !UndefSpecs).

:- pred expand_any_var_references_loop(options_variables::in,
    file_name::in, int::in, list(char)::in, list(char)::in, list(char)::out,
    list(error_spec)::in, list(error_spec)::out,
    set(string)::in, set(string)::out, io::di, io::uo) is det.

expand_any_var_references_loop(_, _, _,
        [], !RevChars, !ParseSpecs, !UndefVarNames, !IO).
expand_any_var_references_loop(Variables, FileName, LineNumber,
        [Char | Chars], !RevChars, !ParseSpecs, !UndefVarNames, !IO) :-
    ( if Char = '$' then
        (
            Chars = [],
            Spec = report_unterminated_variable_reference(FileName, LineNumber,
                !.RevChars),
            !:ParseSpecs = [Spec | !.ParseSpecs]
        ;
            Chars = [Char2 | Chars1],
            ( if Char2 = '$' then
                !:RevChars = ['$' | !.RevChars],
                expand_any_var_references_loop(Variables, FileName, LineNumber,
                    Chars1, !RevChars, !ParseSpecs, !UndefVarNames, !IO)
            else
                ( if
                    ( Char2 = '(', EndChar = ')'
                    ; Char2 = '{', EndChar = '}'
                    )
                then
                    parse_variable_name(FileName, LineNumber, Chars1, Chars2,
                        MaybeVarName0),
                    (
                        MaybeVarName0 = ovos_spec(_),
                        MaybeVarName = MaybeVarName0,
                        ( if Chars2 = [EndChar | Chars3] then
                            Chars4 = Chars3
                        else
                            Chars4 = Chars2
                        )
                    ;
                        MaybeVarName0 = ovos_var_name(_),
                        ( if Chars2 = [EndChar | Chars3] then
                            Chars4 = Chars3,
                            MaybeVarName = MaybeVarName0
                        else
                            Chars4 = Chars2,
                            RefSpec = report_unterminated_variable_reference(
                                FileName, LineNumber, !.RevChars),
                            MaybeVarName = ovos_spec(RefSpec)
                        )
                    )
                else
                    Chars4 = Chars1,
                    VarName0 = string.char_to_string(Char2),
                    MaybeVarName = ovos_var_name(VarName0)
                ),
                (
                    MaybeVarName = ovos_var_name(VarName),
                    lookup_variable_value(Variables, VarName, VarValueChars,
                        !UndefVarNames),
                    !:RevChars = list.reverse(VarValueChars) ++ !.RevChars
                ;
                    MaybeVarName = ovos_spec(ParseSpec),
                    % There is no well-formed variable name to look up.
                    % We could try to put the characters that compose
                    % the malformed variable name to !RevChars, but I (zs)
                    % don't see any point.
                    !:ParseSpecs = [ParseSpec | !.ParseSpecs]
                ),
                expand_any_var_references_loop(Variables, FileName, LineNumber,
                    Chars4, !RevChars, !ParseSpecs, !UndefVarNames, !IO)
            )
        )
    else
        !:RevChars = [Char | !.RevChars],
        expand_any_var_references_loop(Variables, FileName, LineNumber,
            Chars, !RevChars, !ParseSpecs, !UndefVarNames, !IO)
    ).

:- func report_unterminated_variable_reference(file_name, int, list(char))
    = error_spec.

report_unterminated_variable_reference(FileName, LineNumber, RevChars)
        = Spec :-
    Context = term_context.context_init(FileName, LineNumber),
    Pieces = [words("Error: unterminated reference to a variable after"),
        quote(string.from_rev_char_list(RevChars)), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_read_files,
        Context, Pieces).

:- pred report_any_undefined_variables(file_name::in, int::in,
    set(string)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_any_undefined_variables(FileName, LineNumber, UndefVarNamesSet,
        !UndefSpecs) :-
    set.to_sorted_list(UndefVarNamesSet, UndefVarNames),
    (
        UndefVarNames = []
    ;
        ( UndefVarNames = [_],        VarVars = "variable",  IsAre = "is"
        ; UndefVarNames = [_, _ | _], VarVars = "variables", IsAre = "are"
        ),
        UndefVarNamesPieces = list_to_quoted_pieces(UndefVarNames),
        Context = term_context.context_init(FileName, LineNumber),
        Pieces = [words("Warning:"), words(VarVars) | UndefVarNamesPieces] ++
            [words(IsAre), words("undefined."), nl],
        Spec = simplest_spec($pred, severity_warning, phase_read_files,
            Context, Pieces),
        !:UndefSpecs = [Spec | !.UndefSpecs]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

lookup_main_target(Variables, MaybeMainTarget) :-
    lookup_variable_words(Variables, "MAIN_TARGET", MainTargetResult),
    (
        MainTargetResult = var_result_set(MainTarget),
        MaybeMainTarget = ok1(MainTarget)
    ;
        MainTargetResult = var_result_unset,
        MaybeMainTarget = ok1([])
    ;
        MainTargetResult = var_result_error(OoMSpecs),
        MaybeMainTarget = error1(one_or_more_to_list(OoMSpecs))
    ).

%---------------------------------------------------------------------------%

lookup_mercury_stdlib_dir(Variables, MaybeMerStdlibDir) :-
    lookup_variable_words(Variables, "MERCURY_STDLIB_DIR", MerStdlibDirResult),
    (
        MerStdlibDirResult = var_result_set(MerStdlibDir),
        MaybeMerStdlibDir = ok1(MerStdlibDir)
    ;
        MerStdlibDirResult = var_result_unset,
        MaybeMerStdlibDir = ok1([])
    ;
        MerStdlibDirResult = var_result_error(OoMSpecs),
        MaybeMerStdlibDir = error1(one_or_more_to_list(OoMSpecs))
    ).

%---------------------------------------------------------------------------%

lookup_default_options(Variables, Result) :-
    lookup_mmc_maybe_module_options(Variables, default, Result).

lookup_mmc_options(Variables, Result) :-
    lookup_mmc_maybe_module_options(Variables, non_module_specific, Result).

lookup_mmc_module_options(Variables, ModuleName, Result) :-
    lookup_mmc_maybe_module_options(Variables, module_specific(ModuleName),
        Result).

:- pred lookup_mmc_maybe_module_options(options_variables::in,
    options_variable_class::in, maybe1(list(string))::out) is det.

lookup_mmc_maybe_module_options(Variables, MaybeModuleName, Result) :-
    VariableTypes = options_variable_types,
    list.map_foldl(
        lookup_options_variable(Variables, MaybeModuleName),
        VariableTypes, VariableTypesMaybeValues, [], Specs),
    (
        Specs = [],
        MmcOptLists =
            list.map(convert_to_mmc_options, VariableTypesMaybeValues),
        list.condense(MmcOptLists, MmcOpts),
        Result = ok1(MmcOpts)
    ;
        Specs = [_ | _],
        % Returning error1 here is correct because all error_specs in Specs
        % will have severity_error. There is (as of 2022 jan 23) exactly
        % one place in this module that generates an error_spec whose
        % severity is NOT severity_error, but it is not reachable from
        % lookup_options_variable.
        Result = error1(Specs)
    ).

:- type options_variable_class
    --->    default
    ;       non_module_specific
    ;       module_specific(module_name).

:- type options_variable_type
    --->    grade_flags
    ;       mmc_flags
    ;       c_flags
    ;       gcc_flags
    ;       clang_flags
    ;       msvc_flags
    ;       java_flags
    ;       csharp_flags
    ;       ml_objs
    ;       ml_libs
    ;       ld_flags
    ;       ld_libflags
    ;       c2init_args
    ;       libraries
    ;       lib_dirs
    ;       lib_grades
    ;       lib_linkages
    ;       install_prefix
    ;       stdlib_dir
    ;       config_dir
    ;       linkage
    ;       mercury_linkage.

:- func options_variable_types = list(options_variable_type).

options_variable_types =
    % `LIBRARIES' should come before `MLLIBS' (Mercury libraries depend on
    % C libraries, but C libraries typically do not depend on Mercury
    % libraries).
    % `MERCURY_STDLIB_DIR' and `MERCURY_CONFIG_DIR' should come before
    % `MCFLAGS'. Settings in `MCFLAGS' (e.g. `--no-mercury-stdlib-dir')
    % should override settings of these in the environment.
    [grade_flags, linkage, mercury_linkage, lib_grades, lib_linkages,
    stdlib_dir, config_dir, mmc_flags, c_flags, gcc_flags, clang_flags,
    msvc_flags, java_flags, csharp_flags,
    ml_objs, lib_dirs, ld_flags, ld_libflags,
    libraries, ml_libs, c2init_args, install_prefix].

:- func options_variable_name(options_variable_type) = string.

options_variable_name(grade_flags) = "GRADEFLAGS".
options_variable_name(mmc_flags) = "MCFLAGS".
options_variable_name(c_flags) = "CFLAGS".
options_variable_name(gcc_flags) = "GCC_FLAGS".
options_variable_name(clang_flags) = "CLANG_FLAGS".
options_variable_name(msvc_flags) = "MSVC_FLAGS".
options_variable_name(java_flags) = "JAVACFLAGS".
options_variable_name(csharp_flags) = "CSCFLAGS".
options_variable_name(ml_objs) = "MLOBJS".
options_variable_name(ml_libs) = "MLLIBS".
options_variable_name(ld_flags) = "LDFLAGS".
options_variable_name(ld_libflags) = "LD_LIBFLAGS".
options_variable_name(c2init_args) = "C2INITARGS".
options_variable_name(libraries) = "LIBRARIES".
options_variable_name(lib_dirs) = "LIB_DIRS".
options_variable_name(lib_grades) = "LIBGRADES".
options_variable_name(lib_linkages) = "LIB_LINKAGES".
options_variable_name(install_prefix) = "INSTALL_PREFIX".
options_variable_name(stdlib_dir) = "MERCURY_STDLIB_DIR".
options_variable_name(config_dir) = "MERCURY_CONFIG_DIR".
options_variable_name(linkage) = "LINKAGE".
options_variable_name(mercury_linkage) = "MERCURY_LINKAGE".

:- func options_variable_type_is_target_specific(options_variable_type) = bool.

options_variable_type_is_target_specific(grade_flags) = no.
options_variable_type_is_target_specific(mmc_flags) = yes.
options_variable_type_is_target_specific(c_flags) = yes.
options_variable_type_is_target_specific(gcc_flags) = yes.
options_variable_type_is_target_specific(clang_flags) = yes.
options_variable_type_is_target_specific(msvc_flags) = yes.
options_variable_type_is_target_specific(java_flags) = yes.
options_variable_type_is_target_specific(csharp_flags) = yes.
options_variable_type_is_target_specific(ml_objs) = yes.
options_variable_type_is_target_specific(ml_libs) = yes.
options_variable_type_is_target_specific(ld_flags) = yes.
options_variable_type_is_target_specific(ld_libflags) = yes.
options_variable_type_is_target_specific(c2init_args) = yes.
options_variable_type_is_target_specific(libraries) = yes.
options_variable_type_is_target_specific(lib_dirs) = no.
options_variable_type_is_target_specific(install_prefix) = yes.
options_variable_type_is_target_specific(stdlib_dir) = no.
options_variable_type_is_target_specific(config_dir) = no.
options_variable_type_is_target_specific(lib_grades) = yes.
options_variable_type_is_target_specific(lib_linkages) = yes.
options_variable_type_is_target_specific(linkage) = yes.
options_variable_type_is_target_specific(mercury_linkage) = yes.

:- func convert_to_mmc_options(
    pair(options_variable_type, maybe(list(string)))) = list(string).

convert_to_mmc_options(_ - no) = [].
convert_to_mmc_options(VariableType - yes(VariableValue)) =
    convert_to_mmc_options_with_value(VariableType, VariableValue).

:- func convert_to_mmc_options_with_value(options_variable_type, list(string))
    = list(string).

convert_to_mmc_options_with_value(VariableType, VariableValue)
        = OptionsStrings :-
    MMCOptionType = mmc_option_type(VariableType),
    (
        MMCOptionType = mmc_flags,
        OptionsStrings = VariableValue
    ;
        MMCOptionType = option(InitialOptions, OptionName),
        OptionsStrings = list.condense([InitialOptions |
            list.map((func(Word) = [OptionName, Word]), VariableValue)])
    ).

:- type mmc_option_type
    --->    mmc_flags
            % The options can be passed directly to mmc.

    ;       option(
                initial_options :: list(string),
                option_name :: string
            ).
            % The options need to be passed as an argument of an option to mmc.
            % The `initial_options' will be passed before the options generated
            % by the variable. This is useful for clearing an accumulating
            % option.

:- func mmc_option_type(options_variable_type) = mmc_option_type.

mmc_option_type(grade_flags) = mmc_flags.
mmc_option_type(mmc_flags) = mmc_flags.
mmc_option_type(c_flags) = option([], "--cflag").
mmc_option_type(gcc_flags) = option([], "--gcc-flag").
mmc_option_type(clang_flags) = option([], "--clang-flag").
mmc_option_type(msvc_flags) = option([], "--msvc-flag").
mmc_option_type(java_flags) = option([], "--java-flag").
mmc_option_type(csharp_flags) = option([], "--csharp-flag").
mmc_option_type(ml_objs) = option([], "--link-object").
mmc_option_type(ml_libs) = mmc_flags.
mmc_option_type(ld_flags) = option([], "--ld-flag").
mmc_option_type(ld_libflags) = option([], "--ld-libflag").
mmc_option_type(c2init_args) = option([], "--init-file").
mmc_option_type(libraries) = option([], "--mercury-library").
mmc_option_type(lib_dirs) = option([], "--mercury-library-directory").
mmc_option_type(lib_grades) = option(["--no-libgrade"], "--libgrade").
mmc_option_type(lib_linkages) = option(["--no-lib-linkage"], "--lib-linkage").
mmc_option_type(install_prefix) = option([], "--install-prefix").
mmc_option_type(stdlib_dir) = option([], "--mercury-stdlib-dir").
mmc_option_type(config_dir) = option([], "--mercury-config-dir").
mmc_option_type(linkage) = option([], "--linkage").
mmc_option_type(mercury_linkage) = option([], "--mercury-linkage").

%---------------------------------------------------------------------------%

:- type variable_result(T)
    --->    var_result_set(T)
    ;       var_result_unset
    ;       var_result_error(one_or_more(error_spec)).

:- pred lookup_options_variable(options_variables::in,
    options_variable_class::in, options_variable_type::in,
    pair(options_variable_type, maybe(list(string)))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

lookup_options_variable(Variables, OptionsVariableClass, FlagsVar,
        FlagsVar - MaybeValues, !Specs) :-
    VarName = options_variable_name(FlagsVar),
    lookup_variable_words(Variables, "DEFAULT_" ++ VarName,
        DefaultFlagsResult),
    (
        OptionsVariableClass = default,
        FlagsResult = var_result_unset,
        ExtraFlagsResult = var_result_unset
    ;
        ( OptionsVariableClass = module_specific(_)
        ; OptionsVariableClass = non_module_specific
        ),
        lookup_variable_words(Variables, VarName, FlagsResult),
        lookup_variable_words(Variables, "EXTRA_" ++ VarName, ExtraFlagsResult)
    ),
    ( if
        OptionsVariableClass = module_specific(ModuleName),
        options_variable_type_is_target_specific(FlagsVar) = yes
    then
        ModuleFileNameBase = sym_name_to_string(ModuleName),
        ModuleVarName = VarName ++ "-" ++ ModuleFileNameBase,
        lookup_variable_words(Variables, ModuleVarName, ModuleFlagsResult)
    else
        ModuleFlagsResult = var_result_unset
    ),

    % NOTE: The order in which these lists of flags are added together is
    % important. In the resulting set the flags from DefaultFlagsResult
    % *must* occur before those in FlagsResult, which in turn *must* occur
    % before those in ExtraFlagsResult ... etc. Failing to maintain this order
    % will result in the user being unable to override the default value
    % of many of the compiler's options.

    Result =
        DefaultFlagsResult  `combine_var_results`
        FlagsResult         `combine_var_results`
        ExtraFlagsResult    `combine_var_results`
        ModuleFlagsResult,

    % Check the result is valid for the variable type.
    (
        Result = var_result_unset,
        MaybeValues = no
    ;
        Result = var_result_set(Values),
        ( if FlagsVar = ml_libs then
            NotLibLPrefix =
                ( pred(LibFlag::in) is semidet :-
                    not string.prefix(LibFlag, "-l")
                ),
            BadLibs = list.filter(NotLibLPrefix, Values),
            (
                BadLibs = [],
                MaybeValues = yes(Values)
            ;
                BadLibs = [_ | _],
                MaybeValues = no,
                Pieces = [words("Error: MLLIBS must contain only"),
                    quote("-l"), words("options, found") |
                    list_to_pieces(
                        list.map(func(Lib) = add_quotes(Lib), BadLibs))]
                    ++ [suffix(".")],
                Spec = simplest_no_context_spec($pred, severity_error,
                    phase_read_files, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        else
            MaybeValues = yes(Values)
        )
    ;
        Result = var_result_error(OoMSpecs),
        MaybeValues = no,
        !:Specs = one_or_more_to_list(OoMSpecs) ++ !.Specs
    ).

:- func combine_var_results(variable_result(list(T)), variable_result(list(T)))
    = variable_result(list(T)).

combine_var_results(ResultA, ResultB) = Result :-
    (
        ResultA = var_result_unset,
        Result = ResultB
    ;
        ResultA = var_result_set(SA),
        (
            ResultB = var_result_unset,
            Result = ResultA
        ;
            ResultB = var_result_set(SB),
            Result = var_result_set(SA ++ SB)
        ;
            ResultB = var_result_error(_),
            Result = ResultB
        )
    ;
        ResultA = var_result_error(EA),
        (
            ( ResultB = var_result_unset
            ; ResultB = var_result_set(_)
            ),
            Result = ResultA
        ;
            ResultB = var_result_error(EB),
            Result = var_result_error(EA ++ EB)
        )
    ).

:- pred lookup_variable_words(options_variables::in, options_variable::in,
    variable_result(list(string))::out) is det.

lookup_variable_words(Variables, VarName, Result) :-
    Variables = options_variables(OptsMap, EnvMap),
    ( if map.search(EnvMap, VarName, EnvValue) then
        SplitResult = split_into_words(string.to_char_list(EnvValue)),
        (
            SplitResult = ok(EnvWords),
            Result = var_result_set(EnvWords)
        ;
            SplitResult = error(Msg),
            Pieces = [words("Error: in environment variable"),
                quote(VarName), suffix(":"), words(Msg), nl],
            ErrorSpec = simplest_no_context_spec($pred, severity_error,
                phase_read_files, Pieces),
            Result = var_result_error(one_or_more(ErrorSpec, []))
        )
    else
        ( if map.search(OptsMap, VarName, MapValue) then
            MapValue = options_variable_value(_, Words, _),
            Result = var_result_set(Words)
        else
            Result = var_result_unset
        )
    ).

:- pred lookup_variable_value(options_variables::in,
    string::in, list(char)::out, set(string)::in, set(string)::out) is det.

lookup_variable_value(Variables, VarName, ValueChars, !UndefVarNames) :-
    Variables = options_variables(OptsMap, EnvMap),
    ( if map.search(EnvMap, VarName, EnvValue) then
        ValueChars = string.to_char_list(EnvValue)
    else
        ( if map.search(OptsMap, VarName, Entry) then
            Entry = options_variable_value(ValueChars, _, _)
        else
            ValueChars = [],
            set.insert(VarName, !UndefVarNames)
        )
    ).

%---------------------------------------------------------------------------%

dump_options_file(DebugStream, FileName, Variables, !IO) :-
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(DumpStream),
        write_options_variables(DumpStream, Variables, !IO),
        io.close_output(DumpStream, !IO)
    ;
        OpenResult = error(Error),
        ErrorMsg = io.error_message(Error),
        io.format(DebugStream, "mercury_compile: %s\n", [s(ErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ).

    % Write out the given database to the given stream. Intended only
    % for testing the functionality of code that builds such databases.
    %
:- pred write_options_variables(io.text_output_stream::in,
    options_variables::in, io::di, io::uo) is det.

write_options_variables(DumpStream, Variables, !IO) :-
    Variables = options_variables(OptsMap, _EnvMap),
    map.foldl(write_options_variable_value(DumpStream), OptsMap, !IO).
    % tests/options_file/basic_test depends on dumping only OptsMap.
    % You can uncomment this call for debugging.
    % map.foldl(write_env_variable_value(DumpStream), EnvMap, !IO).

:- pred write_options_variable_value(io.text_output_stream::in,
    string::in, options_variable_value::in, io::di, io::uo) is det.

write_options_variable_value(DumpStream, VarName, OptVarValue, !IO) :-
    % The contents of _ValueChars is implicit in ValueWords, so
    % printing it out would just clutter the output and make it
    % harder to read.
    OptVarValue = options_variable_value(_ValueChars, ValueWords, Src),
    io.format(DumpStream, "%-24s ", [s(VarName ++ " ->")], !IO),
    io.write(DumpStream, Src, !IO),
    io.write_string(DumpStream, " ", !IO),
    io.write_line(DumpStream, ValueWords, !IO).

:- pred write_env_variable_value(io.text_output_stream::in,
    string::in, string::in, io::di, io::uo) is det.
:- pragma consider_used(pred(write_env_variable_value/5)).

write_env_variable_value(DumpStream, VarName, VarValue, !IO) :-
    % The contents of _ValueChars is implicit in ValueWords, so
    % printing it out would just clutter the output and make it
    % harder to read.
    io.format(DumpStream, "%-24s %s\n",
        [s(VarName ++ " ->"), s(VarValue)], !IO).

%---------------------------------------------------------------------------%
:- end_module make.options_file.
%---------------------------------------------------------------------------%
