%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: options_file.m.
% Main author: stayl.
%
% Code to deal with options for `mmc --make', including code to parse
% Mercury.options files.
%
%-----------------------------------------------------------------------------%

:- module make.options_file.
:- interface.

:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type options_variables.

:- func options_variables_init = options_variables.

    % Read a single options file, without searching
    % --options-search-directories.
    % This is used to read the configuration file.
    %
:- pred read_options_file(globals::in, file_name::in, options_variables::in,
    maybe(options_variables)::out, io::di, io::uo) is det.

    % Read a single options file. No searching will be done. The result is
    % the value of the variable MCFLAGS obtained from the file, ignoring
    % settings in the environment. This is used to pass arguments to child
    % mmc processes without exceeding command line limits on crappy operating
    % systems.
    %
    % This is not quite the same as @file syntax as the environment is ignored.
    %
:- pred read_args_file(globals::in, file_name::in, maybe(list(string))::out,
    io::di, io::uo) is det.

    % Read all options files specified by `--options-file' options.
    %
:- pred read_options_files(globals::in, options_variables::in,
    maybe(options_variables)::out, io::di, io::uo) is det.

    % Look up the DEFAULT_MCFLAGS variable.
    %
:- pred lookup_default_options(globals::in, options_variables::in,
    maybe(list(string))::out, io::di, io::uo) is det.

    % Look up all the non-module specific options.
    %
:- pred lookup_mmc_options(globals::in, options_variables::in,
    maybe(list(string))::out, io::di, io::uo) is det.

    % Same as lookup_mmc_options, but also adds the module-specific
    % (MCFLAGS-module) options.
    %
:- pred lookup_mmc_module_options(globals::in, options_variables::in,
    module_name::in, maybe(list(string))::out, io::di, io::uo) is det.

    % Look up $(MAIN_TARGET).
    %
:- pred lookup_main_target(globals::in, options_variables::in,
    maybe(list(string))::out, io::di, io::uo) is det.

    % Look up $(MERCURY_STDLIB_DIR).
    %
:- pred lookup_mercury_stdlib_dir(globals::in, options_variables::in,
    maybe(list(string))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module exception.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type options_variable == string.

:- type options_file_error
    --->    options_file_error(string).

:- type found_options_file_error
    --->    found_options_file_error.

:- type options_variables == map(options_variable, options_variable_value).

:- type options_variable_value
    --->    options_variable_value(
                list(char),
                list(string),   % split into words.
                variable_source
            ).

:- type variable_source
    --->    options_file
    ;       command_line
    ;       environment.

options_variables_init = map.init.

read_args_file(Globals, OptionsFile, MaybeMCFlags, !IO) :-
    read_options_file(Globals, OptionsFile, options_variables_init,
        MaybeVariables, !IO),
    (
        MaybeVariables = yes(Variables),
        % Ignore settings in the environment -- the parent mmc process
        % will have included those in the file.
        lookup_variable_words_maybe_env(no, Variables, "MCFLAGS", FlagsResult,
            !IO),
        (
            FlagsResult = var_result_set(MCFlags),
            MaybeMCFlags = yes(MCFlags)
        ;
            FlagsResult = var_result_unset,
            io.write_string("mercury_compile: internal error: " ++
                "arguments file does not set MCFLAGS.\n", !IO),
            MaybeMCFlags = no
        ;
            FlagsResult = var_result_error(ErrorSpec),
            MaybeMCFlags = no,
            write_error_spec(ErrorSpec, Globals, 0, _, 0, _, !IO)
        )
    ;
        MaybeVariables = no,
        MaybeMCFlags = no
    ).

read_options_file(Globals, OptionsFile, Variables0, MaybeVariables, !IO) :-
    promise_equivalent_solutions [OptionsFileResult, !:IO] (
        try_io(
            ( pred((Variables1)::out, !.IO::di, !:IO::uo) is det :-
                read_options_file_params(Globals, error, no_search, no,
                    OptionsFile, Variables0, Variables1, !IO)
            ),
            OptionsFileResult, !IO)
    ),
    (
        OptionsFileResult = succeeded(Variables),
        MaybeVariables = yes(Variables)
    ;
        OptionsFileResult = exception(Exception),
        ( if Exception = univ(found_options_file_error) then
            MaybeVariables = no
        else
            rethrow(OptionsFileResult)
        )
    ).

read_options_files(Globals, Variables0, MaybeVariables, !IO) :-
    promise_equivalent_solutions [OptionsFileResult, !:IO] (
        try_io(read_options_file_lookup_params(Globals, Variables0),
            OptionsFileResult, !IO)
    ),
    (
        OptionsFileResult = succeeded(Variables),
        MaybeVariables = yes(Variables)
    ;
        OptionsFileResult = exception(Exception),
        ( if Exception = univ(found_options_file_error) then
            MaybeVariables = no
        else
            rethrow(OptionsFileResult)
        )
    ).

:- pred read_options_file_lookup_params(globals::in,
    options_variables::in, options_variables::out, io::di, io::uo) is det.

read_options_file_lookup_params(Globals, !Variables, !IO) :-
    globals.lookup_accumulating_option(Globals, options_files, OptionsFiles),
    list.foldl2(read_options_file_set_params(Globals), OptionsFiles,
        !Variables, !IO).

:- pred read_options_file_set_params(globals::in, string::in,
    options_variables::in, options_variables::out, io::di, io::uo) is det.

read_options_file_set_params(Globals, OptionsFile, !Vars, !IO) :-
    ( if OptionsFile = "Mercury.options" then
        ErrorIfNotExist = no_error,
        Search = no_search
    else
        ErrorIfNotExist = error,
        Search = search
    ),
    read_options_file_params(Globals, ErrorIfNotExist, Search, no, OptionsFile,
        !Vars, !IO).

:- type error_if_not_exist
    --->    error
    ;       no_error.

:- type search
    --->    search
    ;       no_search.

:- pred read_options_file_params(globals::in, error_if_not_exist::in,
    search::in, maybe(dir_name)::in, string::in, options_variables::in,
    options_variables::out, io::di, io::uo) is det.

read_options_file_params(Globals, ErrorIfNotExist, Search, MaybeDirName,
        OptionsFile0, !Variables, !IO) :-
    ( if OptionsFile0 = "-" then
        % Read from standard input.
        debug_make_msg(Globals, write_reading_options_file_stdin, !IO),
        read_options_lines(Globals, dir.this_directory, !Variables, !IO),
        debug_make_msg(Globals, write_done, !IO)
    else
        debug_make_msg(Globals, write_reading_options_file(OptionsFile0), !IO),
        (
            Search = search,
            globals.lookup_accumulating_option(Globals,
                options_search_directories, SearchDirs)
        ;
            Search = no_search,
            SearchDirs = [dir.this_directory]
        ),
        ( if dir.split_name(OptionsFile0, OptionsDir, OptionsFile) then
            ( if dir.path_name_is_absolute(OptionsDir) then
                FileToFind = OptionsFile,
                Dirs = [OptionsDir]
            else
                (
                    MaybeDirName = yes(DirName),
                    FileToFind = OptionsFile,
                    Dirs = [DirName/OptionsDir | SearchDirs]
                ;
                    MaybeDirName = no,
                    Dirs = SearchDirs,
                    FileToFind = OptionsFile0
                )
            )
        else
            Dirs = SearchDirs,
            FileToFind = OptionsFile0
        ),
        search_for_file_returning_dir_and_stream(Dirs, FileToFind,
            MaybeDirAndStream, !IO),
        (
            MaybeDirAndStream =
                ok(path_name_and_stream(FoundDir, FoundStream)),
            debug_make_msg(Globals,
                write_reading_options_file(FoundDir/FileToFind), !IO),

            % XXX Instead of setting and unsetting the input stream,
            % we should simply pass FoundStream to read_options_lines.
            % However, when I (zs) tried that, I quickly found that
            % the call tree of read_options_lines includes many predicates
            % for which it is not at all clear whether they *intend*
            % to read from a current standard input that originates as
            % FoundStream, or they just *happen* to do so.
            %
            % XXX The changeover would also be simpler if there was an easy way
            % to detect calls that read from the current input stream.

            io.set_input_stream(FoundStream, OldInputStream, !IO),
            read_options_lines(Globals, FoundDir, !Variables, !IO),
            io.set_input_stream(OldInputStream, _FoundStream, !IO),
            io.close_input(FoundStream, !IO)
        ;
            MaybeDirAndStream = error(_),
            (
                ErrorIfNotExist = error,
                ( if Dirs = [SingleDir] then
                    ErrorFile = maybe_add_path_name(SingleDir, FileToFind)
                else
                    ErrorFile = FileToFind
                ),
                ErrorSpec = error_spec(severity_error, phase_read_files,
                    [error_msg(no, do_not_treat_as_first, 0,
                        [always([words("Error reading options file"),
                            quote(ErrorFile), suffix(".")])])]),
                write_error_spec(ErrorSpec, Globals, 0, _, 0, _, !IO)
            ;
                ErrorIfNotExist = no_error
            )
        ),
        debug_make_msg(Globals, write_done, !IO)
    ).

:- pred write_reading_options_file_stdin(io::di, io::uo) is det.

write_reading_options_file_stdin(!IO) :-
    io.write_string("Reading options file from stdin...", !IO).

:- pred write_reading_options_file(string::in, io::di, io::uo) is det.

write_reading_options_file(FileName, !IO) :-
    io.write_string("Reading options file ", !IO),
    io.write_string(FileName, !IO),
    io.nl(!IO).

:- pred write_done(io::di, io::uo) is det.

write_done(!IO) :-
    io.write_string("done.\n", !IO).

:- func maybe_add_path_name(dir_name, file_name) = file_name.

maybe_add_path_name(Dir, File) =
    ( if Dir = dir.this_directory then
        File
    else
        dir.make_path_name(Dir, File)
    ).

:- pred read_options_lines(globals::in, dir_name::in, options_variables::in,
    options_variables::out, io::di, io::uo) is det.

read_options_lines(Globals, Dir, !Variables, !IO) :-
    io.get_line_number(LineNumber, !IO),
    promise_equivalent_solutions [LineResult, !:IO] (
        read_options_lines_2(Globals, Dir, !.Variables, LineResult, !IO)
    ),
    (
        LineResult = succeeded(!:Variables - FoundEOF),
        (
            FoundEOF = yes
        ;
            FoundEOF = no,
            read_options_lines(Globals, Dir, !Variables, !IO)
        )
    ;
        LineResult = exception(Exception),
        ( if Exception = univ(options_file_error(Error)) then
            io.input_stream_name(FileName, !IO),
            Context = term.context_init(FileName, LineNumber),
            write_error_pieces(Globals, Context, 0, [words(Error)], !IO),

            % This will be caught by `read_options_files'. The open options
            % files aren't closed on the way up, but we will be exiting
            % straight away, so that doesn't matter.
            throw(found_options_file_error)
        else
            rethrow(LineResult)
        )
    ;
        LineResult = failed,
        unexpected($module, $pred, "cannot read line")
    ).

:- pred read_options_lines_2(globals::in, dir_name::in, options_variables::in,
    exception_result(pair(options_variables, bool))::out,
    io::di, io::uo) is cc_multi.

read_options_lines_2(Globals, Dir, Variables0, Result, !IO) :-
    try_io(read_options_lines_3(Globals, Dir, Variables0), Result, !IO).

:- pred read_options_lines_3(globals::in, dir_name::in, options_variables::in,
    pair(options_variables, bool)::out, io::di, io::uo) is det.

read_options_lines_3(Globals, Dir, !.Variables, !:Variables - FoundEOF, !IO) :-
    read_options_line(FoundEOF, Line0, !IO),
    (
        Line0 = []
    ;
        Line0 = [_ | _],
        parse_options_line(Line0, ParsedLine),
        (
            ParsedLine = define_variable(VarName, AddToValue, Value),
            update_variable(Globals, VarName, AddToValue, Value,
                !Variables, !IO)
        ;
            ParsedLine = include_options_files(ErrorIfNotExist,
                IncludedFilesChars0),
            expand_variables(!.Variables,
                IncludedFilesChars0, IncludedFilesChars, UndefVars, !IO),
            report_undefined_variables(Globals, UndefVars, !IO),
            IncludedFileNames = split_into_words(IncludedFilesChars),
            list.foldl2(
                read_options_file_params(Globals, ErrorIfNotExist, search,
                    yes(Dir)),
                IncludedFileNames, !Variables, !IO)
        )
    ).

:- pred read_options_line(bool::out, list(char)::out, io::di, io::uo) is det.

read_options_line(FoundEOF, list.reverse(RevChars), !IO) :-
    io.ignore_whitespace(SpaceResult, !IO),
    (
        SpaceResult = ok
    ;
        SpaceResult = eof
    ;
        SpaceResult = error(Error),
        throw(options_file_error(io.error_message(Error)))
    ),
    read_options_line_2(FoundEOF, [], RevChars, !IO).

:- pred read_options_line_2(bool::out, list(char)::in, list(char)::out,
    io::di, io::uo) is det.

read_options_line_2(FoundEOF, !Chars, !IO) :-
    read_item_or_eof(io.read_char, MaybeChar, !IO),
    (
        MaybeChar = yes(Char),
        ( if Char = '#' then
            skip_comment_line(FoundEOF, !IO)
        else if Char = ('\\') then
            read_item_or_eof(io.read_char, MaybeChar2, !IO),
            (
                MaybeChar2 = yes(Char2),
                ( if Char2 = '\n' then
                    !:Chars = [' ' | !.Chars]
                else
                    !:Chars = [Char2, Char | !.Chars]
                ),
                read_options_line_2(FoundEOF, !Chars, !IO)
            ;
                MaybeChar2 = no,
                FoundEOF = yes,
                !:Chars = [Char | !.Chars]
            )
        else if Char = '\n' then
            FoundEOF = no
        else
            !:Chars = [Char | !.Chars],
            read_options_line_2(FoundEOF, !Chars, !IO)
        )
    ;
        MaybeChar = no,
        FoundEOF = yes
    ).

:- pred update_variable(globals::in, options_variable::in, bool::in,
    list(char)::in, options_variables::in, options_variables::out,
    io::di, io::uo) is det.

update_variable(Globals, VarName, AddToValue, NewValue0, !Variables, !IO) :-
    expand_variables(!.Variables, NewValue0, NewValue1, Undef, !IO),
    report_undefined_variables(Globals, Undef, !IO),
    Words1 = split_into_words(NewValue1),
    io.get_environment_var(VarName, MaybeEnvValue, !IO),
    (
        MaybeEnvValue = yes(EnvValue),
        Value = string.to_char_list(EnvValue),
        Words = split_into_words(Value),
        OptVarValue = options_variable_value(string.to_char_list(EnvValue),
            Words, environment),
        map.set(VarName, OptVarValue, !Variables)
    ;
        MaybeEnvValue = no,
        ( if
            map.search(!.Variables, VarName,
                options_variable_value(OldValue, OldWords, Source))
        then
            (
                Source = environment
            ;
                Source = command_line
            ;
                Source = options_file,
                (
                    AddToValue = yes,
                    NewValue = OldValue ++ [' ' |  NewValue1],
                    Words = OldWords ++ Words1
                ;
                    AddToValue = no,
                    NewValue = NewValue1,
                    Words = Words1
                ),
                OptVarValue = options_variable_value(NewValue, Words,
                    options_file),
                map.set(VarName, OptVarValue, !Variables)
            )
        else
            OptVarValue = options_variable_value(NewValue1, Words1,
                options_file),
            map.set(VarName, OptVarValue, !Variables)
        )
    ).

:- pred expand_variables(options_variables::in, list(char)::in,
    list(char)::out, list(string)::out, io::di, io::uo) is det.

expand_variables(Variables, Chars0, Chars, UndefVars, !IO) :-
    expand_variables_2(Variables, Chars0, [], RevChars, [], RevUndefVars, !IO),
    list.reverse(RevChars, Chars),
    list.reverse(RevUndefVars, UndefVars).

:- pred expand_variables_2(options_variables::in, list(char)::in,
    list(char)::in, list(char)::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

expand_variables_2(_, [], !RevChars, !RevUndef, !IO).
expand_variables_2(Variables, [Char | Chars], !RevChars, !RevUndef, !IO) :-
    ( if Char = '$' then
        (
            Chars = [],
            throw(options_file_error("unterminated variable reference"))
        ;
            Chars = [Char2 | Chars1],
            ( if Char2 = '$' then
                !:RevChars = ['$' | !.RevChars],
                expand_variables_2(Variables, Chars1, !RevChars, !RevUndef,
                    !IO)
            else
                ( if
                    (
                        Char2 = '(',
                        EndChar = ')'
                    ;
                        Char2 = '{',
                        EndChar = '}'
                    )
                then
                    parse_variable(VarName0, Chars1, Chars2),
                    ( if Chars2 = [EndChar | Chars3] then
                        Chars4 = Chars3,
                        VarName = VarName0
                    else
                        throw(options_file_error(
                            "unterminated variable reference"))
                    )
                else
                    Chars4 = Chars1,
                    VarName = string.char_to_string(Char2)
                ),
                lookup_variable_chars(Variables, VarName, VarChars, !RevUndef,
                    !IO),
                !:RevChars = reverse(VarChars) ++ !.RevChars,
                expand_variables_2(Variables, Chars4, !RevChars, !RevUndef,
                    !IO)
            )
        )
    else
        !:RevChars = [Char | !.RevChars],
        expand_variables_2(Variables, Chars, !RevChars, !RevUndef, !IO)
    ).

:- pred report_undefined_variables(globals::in, list(string)::in,
    io::di, io::uo) is det.

report_undefined_variables(Globals, Vars, !IO) :-
    report_undefined_variables_2(Globals, list.sort_and_remove_dups(Vars),
        !IO).

:- pred report_undefined_variables_2(globals::in, list(string)::in,
    io::di, io::uo) is det.

report_undefined_variables_2(_, [], !IO).
report_undefined_variables_2(Globals, [_ | Rest] @ UndefVars, !IO) :-
    globals.lookup_bool_option(Globals, warn_undefined_options_variables,
        Warn),
    (
        Warn = yes,
        io.input_stream_name(FileName, !IO),
        io.get_line_number(LineNumber, !IO),
        Context = term.context_init(FileName, LineNumber),

        VarList = list_to_pieces(list.map(add_quotes,
            list.sort_and_remove_dups(UndefVars))),
        ( Rest = [], Word = "variable", IsOrAre = "is"
        ; Rest = [_ | _], Word = "variables", IsOrAre = "are"
        ),
        Pieces = [words("Warning: "), words(Word) | VarList]
            ++ [words(IsOrAre), words("undefined.")],
        write_error_pieces(Globals, Context, 0, Pieces, !IO),

        globals.lookup_bool_option(Globals, halt_at_warn, Halt),
        (
            Halt = yes,
            throw(found_options_file_error)
        ;
            Halt = no
        )
    ;
        Warn = no
    ).

%-----------------------------------------------------------------------------%

:- type options_file_line
    --->    define_variable(
                options_variable,
                bool,       % Add to any existing value?
                list(char)
            )
    ;       include_options_files(
                error_if_not_exist,
                list(char)
            ).

:- pred parse_options_line(list(char)::in, options_file_line::out) is det.

parse_options_line(Line0, OptionsFileLine) :-
    ( if
        ( if Line0 = [('-') | Line1] then
            ErrorIfNotExist = no_error,
            Line2 = Line1
        else
            ErrorIfNotExist = error,
            Line2 = Line0
        ),
        list.append(string.to_char_list("include"), Line3, Line2)
    then
        list.drop_while(char.is_whitespace, Line3, Line4),
        OptionsFileLine = include_options_files(ErrorIfNotExist, Line4)
    else
        parse_variable(VarName, Line0, Line1),
        list.drop_while(char.is_whitespace, Line1, Line2),
        ( if Line2 = [('=') | Line3] then
            Add = no,
            Line4 = Line3
        else if Line2 = [('+'), ('=') | Line3] then
            Add = yes,
            Line4 = Line3
        else if Line2 = [(':'), ('=') | Line3] then
            Add = no,
            Line4 = Line3
        else
            throw(options_file_error(
                "expected `=', `:=' or `+=' after `" ++ VarName ++ "'"))
        ),
        list.drop_while(char.is_whitespace, Line4, VarValue),
        OptionsFileLine = define_variable(VarName, Add, VarValue)
    ).

:- pred parse_variable(options_variable::out,
    list(char)::in, list(char)::out) is det.

parse_variable(VarName, Chars0, Chars) :-
    parse_variable_2(yes, [], VarList, Chars0, Chars),
    string.from_rev_char_list(VarList, VarName),
    ( if VarName = "" then
        list.take_while(isnt(char.is_whitespace), Chars, FirstWord),
        throw(options_file_error("expected variable at `" ++
            string.from_char_list(FirstWord) ++ "'"))
    else
        true
    ).

:- pred parse_variable_2(bool::in, list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

parse_variable_2(_, Var, Var, [], []).
parse_variable_2(IsFirst, Var0, Var, [Char | Chars0], Chars) :-
    ( if
        not char.is_whitespace(Char),
        (
            IsFirst = yes,
            char.is_alpha(Char)
        ;
            IsFirst = no,
            ( char.is_alnum_or_underscore(Char)
            ; Char = ('-')
            ; Char = ('.')
            )
        )
    then
        parse_variable_2(no, [Char | Var0], Var, Chars0, Chars)
    else
        Var = Var0,
        Chars = [Char | Chars0]
    ).

:- pred parse_string_chars(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

parse_string_chars(_, _, [], _) :-
    throw(options_file_error("unterminated string")).
parse_string_chars(String0, String, [Char | Chars0], Chars) :-
    ( if Char = '"' then
        Chars = Chars0,
        String = String0
    else if Char = ('\\') then
        (
            Chars0 = [Char2 | Chars1],
            ( if Char2 = '"' then
                String1 = [Char2 | String0]
            else
                String1 = [Char2, Char | String0]
            ),
            parse_string_chars(String1, String, Chars1, Chars)
        ;
            Chars0 = [],
            throw(options_file_error("unterminated string"))
        )
    else
        parse_string_chars([Char | String0], String, Chars0, Chars)
    ).

:- pred skip_comment_line(bool::out, io::di, io::uo) is det.

skip_comment_line(FoundEOF, !IO) :-
    read_item_or_eof(io.read_char, MaybeChar, !IO),
    (
        MaybeChar = yes(Char),
        ( if Char = '\n' then
            FoundEOF = no
        else
            skip_comment_line(FoundEOF, !IO)
        )
    ;
        MaybeChar = no,
        FoundEOF = yes
    ).

:- pred read_item_or_eof(
    pred(io.result(T), io, io)::(pred(out, di, uo) is det),
    maybe(T)::out, io::di, io::uo) is det.

read_item_or_eof(Pred, MaybeItem, !IO) :-
    Pred(Result, !IO),
    (
        Result = ok(Item),
        MaybeItem = yes(Item)
    ;
        Result = eof,
        MaybeItem = no
    ;
        Result = error(Error),
        throw(options_file_error(io.error_message(Error)))
    ).

%-----------------------------------------------------------------------------%

:- func checked_split_into_words(list(char)) = maybe_error(list(string)).

checked_split_into_words(Chars) = Result :-
    promise_equivalent_solutions [TryResult] (
        try(
            ( pred(Words0::out) is det :-
                Words0 = split_into_words(Chars)
            ), TryResult)
    ),
    (
        TryResult = succeeded(Words),
        Result = ok(Words)
    ;
        TryResult = exception(Exception),
        ( if Exception = univ(options_file_error(Msg)) then
            Result = error(Msg)
        else
            rethrow(TryResult)
        )
    ).

:- func split_into_words(list(char)) = list(string).

split_into_words(Chars) = list.reverse(split_into_words_2(Chars, [])).

:- func split_into_words_2(list(char), list(string)) = list(string).

split_into_words_2(Chars0, RevWords0) = RevWords :-
    list.drop_while(char.is_whitespace, Chars0, Chars1),
    (
        Chars1 = [],
        RevWords = RevWords0
    ;
        Chars1 = [_ | _],
        get_word(Word, Chars1, Chars),
        RevWords = split_into_words_2(Chars, [Word | RevWords0])
    ).

:- pred get_word(string::out, list(char)::in, list(char)::out) is det.

get_word(Word, Chars0, Chars) :-
    get_word_2([], RevWord, Chars0, Chars),
    Word = string.from_rev_char_list(RevWord).

:- pred get_word_2(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

get_word_2(RevWord, RevWord, [], []).
get_word_2(RevWord0, RevWord, [Char | Chars0], Chars) :-
    ( if char.is_whitespace(Char) then
        Chars = Chars0,
        RevWord = RevWord0
    else if Char = '"' then
        parse_string_chars([], RevStringChars, Chars0, Chars1),
        get_word_2(RevStringChars ++ RevWord0, RevWord, Chars1, Chars)
    else if Char = ('\\') then
        (
            Chars0 = [],
            RevWord = [Char | RevWord0],
            Chars = []
        ;
            Chars0 = [Char2 | Chars1],
            ( if
                ( Char2 = '"'
                ; Char2 = ('\\')
                )
            then
                get_word_2([Char2 | RevWord0], RevWord, Chars1, Chars)
            else
                get_word_2([Char2, Char | RevWord0], RevWord, Chars1, Chars)
            )
        )
    else
        get_word_2([Char | RevWord0], RevWord, Chars0, Chars)
    ).

%-----------------------------------------------------------------------------%

lookup_main_target(Globals, Vars, MaybeMainTarget, !IO) :-
    lookup_variable_words_report_error(Globals, Vars, "MAIN_TARGET",
        MainTargetResult, !IO),
    (
        MainTargetResult = var_result_set(MainTarget),
        MaybeMainTarget = yes(MainTarget)
    ;
        MainTargetResult = var_result_unset,
        MaybeMainTarget = yes([])
    ;
        MainTargetResult = var_result_error(_),
        MaybeMainTarget = no
    ).

%-----------------------------------------------------------------------------%

lookup_mercury_stdlib_dir(Globals, Vars, MaybeMerStdlibDir, !IO) :-
    lookup_variable_words_report_error(Globals, Vars, "MERCURY_STDLIB_DIR",
        MerStdlibDirResult, !IO),
    (
        MerStdlibDirResult = var_result_set(MerStdlibDir),
        MaybeMerStdlibDir = yes(MerStdlibDir)
    ;
        MerStdlibDirResult = var_result_unset,
        MaybeMerStdlibDir = yes([])
    ;
        MerStdlibDirResult = var_result_error(_),
        MaybeMerStdlibDir = no
    ).

%-----------------------------------------------------------------------------%

lookup_default_options(Globals, Vars, Result, !IO) :-
    lookup_mmc_maybe_module_options(Globals, Vars, default, Result, !IO).

lookup_mmc_options(Globals, Vars, Result, !IO) :-
    lookup_mmc_maybe_module_options(Globals, Vars,
        non_module_specific, Result, !IO).

lookup_mmc_module_options(Globals, Vars, ModuleName, Result, !IO) :-
    lookup_mmc_maybe_module_options(Globals, Vars,
        module_specific(ModuleName), Result, !IO).

:- pred lookup_mmc_maybe_module_options(globals::in, options_variables::in,
    options_variable_class::in, maybe(list(string))::out, io::di, io::uo)
    is det.

lookup_mmc_maybe_module_options(Globals, Vars, MaybeModuleName, Result, !IO) :-
    VariableTypes = options_variable_types,
    list.map_foldl(lookup_options_variable(Globals, Vars, MaybeModuleName),
        VariableTypes, Results, !IO),
    ( if
        list.map(
            ( pred(VarResult::in, MaybeValue::out) is semidet :-
                (
                    VarResult = var_result_set(Value),
                    MaybeValue = yes(Value)
                ;
                    VarResult = var_result_unset,
                    MaybeValue = no
                ;
                    VarResult = var_result_error(_),
                    fail
                )
            ), Results, Values)
    then
        assoc_list.from_corresponding_lists(VariableTypes,
            Values, VariableValues),
        % Default to `-O2', even when mercury_compile is called directly,
        % not by the mmc script.
        Result = yes(["-O2" | list.condense(
            list.map(convert_to_mmc_options, VariableValues))])
    else
        Result = no
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
    ;       erlang_flags
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
    msvc_flags, java_flags,
    csharp_flags, erlang_flags,
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
options_variable_name(erlang_flags) = "ERLANG_FLAGS".
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
options_variable_type_is_target_specific(erlang_flags) = yes.
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
mmc_option_type(erlang_flags) = option([], "--erlang-flag").
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

%-----------------------------------------------------------------------------%

:- type variable_result(T)
    --->    var_result_set(T)
    ;       var_result_unset
    ;       var_result_error(error_spec).

:- pred lookup_options_variable(globals::in, options_variables::in,
    options_variable_class::in, options_variable_type::in,
    variable_result(list(string))::out,
    io::di, io::uo) is det.

lookup_options_variable(Globals, Vars, OptionsVariableClass, FlagsVar, Result,
        !IO) :-
    VarName = options_variable_name(FlagsVar),
    lookup_variable_words_report_error(Globals, Vars, "DEFAULT_" ++ VarName,
        DefaultFlagsResult, !IO),
    (
        OptionsVariableClass = default,
        FlagsResult = var_result_unset,
        ExtraFlagsResult = var_result_unset
    ;
        ( OptionsVariableClass = module_specific(_)
        ; OptionsVariableClass = non_module_specific
        ),
        lookup_variable_words_report_error(Globals, Vars, VarName,
            FlagsResult, !IO),
        lookup_variable_words_report_error(Globals, Vars, "EXTRA_" ++ VarName,
            ExtraFlagsResult, !IO)
    ),
    ( if
        OptionsVariableClass = module_specific(ModuleName),
        options_variable_type_is_target_specific(FlagsVar) = yes
    then
        ModuleFileNameBase = sym_name_to_string(ModuleName),
        ModuleVarName = VarName ++ "-" ++ ModuleFileNameBase,
        lookup_variable_words_report_error(Globals, Vars, ModuleVarName,
            ModuleFlagsResult, !IO)
    else
        ModuleFlagsResult = var_result_unset
    ),

    % NOTE: The order in which these lists of flags are added together is
    % important. In the resulting set the flags from DefaultFlagsResult
    % *must* occur before those in FlagsResult, which in turn *must* occur
    % before those in ExtraFlagsResult ... etc. Failing to maintain this order
    % will result in the user being unable to override the default value
    % of many of the compiler's options.

    Result0 =
        DefaultFlagsResult  `combine_var_results`
        FlagsResult         `combine_var_results`
        ExtraFlagsResult    `combine_var_results`
        ModuleFlagsResult,

    % Check the result is valid for the variable type.
    (
        Result0 = var_result_unset,
        Result = var_result_unset
    ;
        Result0 = var_result_error(E),
        Result = var_result_error(E)
    ;
        Result0 = var_result_set(V),
        ( if FlagsVar = ml_libs then
            NotLibLPrefix =
                ( pred(LibFlag::in) is semidet :-
                    not string.prefix(LibFlag, "-l")
                ),
            BadLibs = list.filter(NotLibLPrefix, V),
            (
                BadLibs = [],
                Result = Result0
            ;
                BadLibs = [_ | _],
                Pieces = [words("Error: MLLIBS must contain only"),
                    quote("-l"), words("options, found") |
                    list_to_pieces(
                        list.map(func(Lib) = add_quotes(Lib), BadLibs))]
                    ++ [suffix(".")],
                ErrorSpec = error_spec(severity_error, phase_read_files,
                    [error_msg(no, do_not_treat_as_first, 0,
                        [always(Pieces)])]),
                write_error_spec(ErrorSpec, Globals, 0, _, 0, _, !IO),
                Result = var_result_error(ErrorSpec)
            )
        else
            Result = Result0
        )
    ).

:- func combine_var_results(variable_result(list(T)), variable_result(list(T)))
    = variable_result(list(T)).

combine_var_results(var_result_unset, var_result_unset) = var_result_unset.
combine_var_results(var_result_unset, var_result_set(V)) = var_result_set(V).
combine_var_results(var_result_unset, var_result_error(E)) =
    var_result_error(E).
combine_var_results(var_result_set(V1), var_result_set(V2)) =
    var_result_set(V1 ++ V2).
combine_var_results(var_result_set(V), var_result_unset) = var_result_set(V).
combine_var_results(var_result_set(_), var_result_error(E)) =
    var_result_error(E).
combine_var_results(var_result_error(E), _) = var_result_error(E).

:- pred lookup_variable_words_report_error(globals::in, options_variables::in,
    options_variable::in, variable_result(list(string))::out,
    io::di, io::uo) is det.

lookup_variable_words_report_error(Globals, Vars, VarName, Result, !IO) :-
    lookup_variable_words(Vars, VarName, Result, !IO),
    (
        Result = var_result_error(ErrorSpec),
        write_error_spec(ErrorSpec, Globals, 0, _, 0, _, !IO)
    ;
        Result = var_result_set(_)
    ;
        Result = var_result_unset
    ).

:- pred lookup_variable_words(options_variables::in, options_variable::in,
    variable_result(list(string))::out, io::di, io::uo) is det.

lookup_variable_words(Vars, VarName, Result, !IO) :-
    lookup_variable_words_maybe_env(yes, Vars, VarName, Result, !IO).

:- pred lookup_variable_words_maybe_env(bool::in, options_variables::in,
    options_variable::in, variable_result(list(string))::out,
    io::di, io::uo) is det.

lookup_variable_words_maybe_env(LookupEnv, Vars, VarName, Result, !IO) :-
    (
        LookupEnv = yes,
        io.get_environment_var(VarName, MaybeEnvValue, !IO)
    ;
        LookupEnv = no,
        MaybeEnvValue = no
    ),
    (
        MaybeEnvValue = yes(EnvValue),
        SplitResult = checked_split_into_words(string.to_char_list(EnvValue)),
        (
            SplitResult = ok(EnvWords),
            Result = var_result_set(EnvWords)
        ;
            SplitResult = error(Msg),
            ErrorSpec = error_spec(severity_error, phase_read_files,
                [error_msg(no, do_not_treat_as_first, 0,
                    [always([words("Error: in environment variable"),
                        quote(VarName), suffix(":"), words(Msg), nl])]
                )]),
            Result = var_result_error(ErrorSpec)
        )
    ;
        MaybeEnvValue = no,
        ( if map.search(Vars, VarName, MapValue) then
            MapValue = options_variable_value(_, Words, _),
            Result = var_result_set(Words)
        else
            Result = var_result_unset
        )
    ).

:- pred lookup_variable_chars(options_variables::in, string::in,
    list(char)::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

lookup_variable_chars(Variables, Var, Value, !Undef, !IO) :-
    io.get_environment_var(Var, MaybeValue, !IO),
    (
        MaybeValue = yes(ValueString),
        Value = string.to_char_list(ValueString)
    ;
        MaybeValue = no,
        ( if
            map.search(Variables, Var, options_variable_value(Value0, _, _))
        then
            Value = Value0
        else
            Value = [],
            !:Undef = [Var | !.Undef]
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module make.options_file.
%-----------------------------------------------------------------------------%
