%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: options_file.m
% Main author: stayl
%
% Code to deal with options for `mmc --make', including code to parse
% an Mmakefile equivalent.
%
%-----------------------------------------------------------------------------%

:- module make__options_file.

:- interface.

:- import_module mdbcomp__prim_data.

:- import_module io.
:- import_module list.
:- import_module std_util.

:- type options_variables.

:- func options_variables_init = options_variables.

    % Read a single options file, without searching
    % --options-search-directories.
    % This is used to read the configuration file.
    %
:- pred read_options_file(file_name::in, options_variables::in,
    maybe(options_variables)::out, io::di, io::uo) is det.

    % Read a single options file. No searching will be done. The result is
    % the value of the variable MCFLAGS obtained from the file, ignoring
    % settings in the environment. This is used to pass arguments to child
    % mmc processes without exceeding command line limits on crappy operating
    % systems.
    %
:- pred read_args_file(file_name::in, maybe(list(string))::out,
    io::di, io::uo) is det.

    % Read all options files specified by `--options-file' options.
    %
:- pred read_options_files(options_variables::in,
    maybe(options_variables)::out, io::di, io::uo) is det.

    % Look up the DEFAULT_MCFLAGS variable.
    %
:- pred lookup_default_options(options_variables::in, maybe(list(string))::out,
    io::di, io::uo) is det.

    % Look up all the non-module specific options.
    %
:- pred lookup_mmc_options(options_variables::in, maybe(list(string))::out,
    io::di, io::uo) is det.

    % Same as lookup_mmc_module_options, but also adds the module-specific
    % (MCFLAGS-module) options.
    %
:- pred lookup_mmc_module_options(options_variables::in, module_name::in,
    maybe(list(string))::out, io::di, io::uo) is det.

    % Look up $(MAIN_TARGET).
    %
:- pred lookup_main_target(options_variables::in, maybe(list(string))::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module exception.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.

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

options_variables_init = map__init.

read_args_file(OptionsFile, MaybeMCFlags, !IO) :-
    read_options_file(OptionsFile, options_variables_init, MaybeVariables,
        !IO),
    (
        MaybeVariables = yes(Variables),
        % Ignore settings in the environment -- the parent
        % mmc process will have included those in the file.
        lookup_variable_words(no, Variables, "MCFLAGS", FlagsResult, !IO),
        (
            FlagsResult = set(MCFlags),
            MaybeMCFlags = yes(MCFlags)
        ;
            FlagsResult = unset,
            io__write_string("mercury_compile: internal error: " ++
                "arguments file does not set MCFLAGS.\n", !IO),
            MaybeMCFlags = no
        ;
            FlagsResult = error(Msg),
            MaybeMCFlags = no,
            io__write_string(Msg, !IO),
            io__nl(!IO)
        )
    ;
        MaybeVariables = no,
        MaybeMCFlags = no
    ).

read_options_file(OptionsFile, Variables0, MaybeVariables, !IO) :-
    promise_equivalent_solutions [OptionsFileResult, !:IO] (
        try_io((pred((Variables1)::out, !.IO::di, !:IO::uo) is det :-
                    read_options_file_params(error, no_search, no, OptionsFile,
                    Variables0, Variables1, !IO)
            ),
            OptionsFileResult, !IO)
    ),
    (
        OptionsFileResult = succeeded(Variables),
        MaybeVariables = yes(Variables)
    ;
        OptionsFileResult = exception(Exception),
        ( Exception = univ(found_options_file_error) ->
            MaybeVariables = no
        ;
            rethrow(OptionsFileResult)
        )
    ).

read_options_files(Variables0, MaybeVariables, !IO) :-
    promise_equivalent_solutions [OptionsFileResult, !:IO] (
        try_io(read_options_file_lookup_params(Variables0), OptionsFileResult,
            !IO)
    ),
    (
        OptionsFileResult = succeeded(Variables),
        MaybeVariables = yes(Variables)
    ;
        OptionsFileResult = exception(Exception),
        ( Exception = univ(found_options_file_error) ->
            MaybeVariables = no
        ;
            rethrow(OptionsFileResult)
        )
    ).

:- pred read_options_file_lookup_params(
    options_variables::in, options_variables::out, io::di, io::uo) is det.

read_options_file_lookup_params(!Variables, !IO) :-
    globals.io_lookup_accumulating_option(options_files, OptionsFiles, !IO),
    list.foldl2(read_options_file_set_params, OptionsFiles, !Variables, !IO).

:- pred read_options_file_set_params(string::in,
    options_variables::in, options_variables::out, io::di, io::uo) is det.

read_options_file_set_params(OptionsFile, !Vars, !IO) :-
    ( OptionsFile = "Mercury.options" ->
        ErrorIfNotExist = no_error,
        Search = no_search
    ;
        ErrorIfNotExist = error,
        Search = search
    ),
    read_options_file_params(ErrorIfNotExist, Search, no, OptionsFile,
        !Vars, !IO).

:- type error_if_not_exist
    --->    error
    ;       no_error.

:- type search
    --->    search
    ;       no_search.

    % read_options_file_params(ErrorIfNotExist, Search, MaybeDirName,
    %   FileName, Variables0, Variables).
    %
:- pred read_options_file_params(error_if_not_exist::in, search::in,
    maybe(dir_name)::in, string::in, options_variables::in,
    options_variables::out, io::di, io::uo) is det.

read_options_file_params(ErrorIfNotExist, Search, MaybeDirName, OptionsFile0,
        !Variables, !IO) :-
    ( OptionsFile0 = "-" ->
        % Read from standard input.
        debug_msg(write_reading_options_file_stdin, !IO),
        read_options_lines(dir__this_directory, !Variables, !IO),
        debug_msg(write_done, !IO)
    ;
        debug_msg(write_reading_options_file(OptionsFile0), !IO),
        (
            Search = search,
            globals__io_lookup_accumulating_option(options_search_directories,
                SearchDirs, !IO)
        ;
            Search = no_search,
            SearchDirs = [dir__this_directory]
        ),
        ( dir__split_name(OptionsFile0, OptionsDir, OptionsFile) ->
            ( dir__path_name_is_absolute(OptionsDir) ->
                FileToFind = OptionsFile,
                Dirs = [OptionsDir]
            ; MaybeDirName = yes(DirName) ->
                FileToFind = OptionsFile,
                Dirs = [DirName/OptionsDir | SearchDirs]
            ;
                Dirs = SearchDirs,
                FileToFind = OptionsFile0
            )
        ;
            Dirs = SearchDirs,
            FileToFind = OptionsFile0
        ),
        io__input_stream(OldInputStream, !IO),
        search_for_file_returning_dir(Dirs, FileToFind, MaybeDir, !IO),
        (
            MaybeDir = ok(FoundDir),
            debug_msg(write_reading_options_file(FoundDir/FileToFind), !IO),

            read_options_lines(FoundDir, !Variables, !IO),
            io__set_input_stream(OldInputStream, OptionsStream, !IO),
            io__close_input(OptionsStream, !IO)
        ;
            MaybeDir = error(_),
            (
                ErrorIfNotExist = error,
                ( Dirs = [SingleDir] ->
                    ErrorFile = maybe_add_path_name(SingleDir, FileToFind)
                ;
                    ErrorFile = FileToFind
                ),
                io__write_string("Error reading options file `", !IO),
                io__write_string(ErrorFile, !IO),
                io__write_string("'.\n", !IO),
                io__set_exit_status(1, !IO)
            ;
                ErrorIfNotExist = no_error
            )
        ),
        debug_msg(write_done, !IO)
    ).

:- pred write_reading_options_file_stdin(io::di, io::uo) is det.

write_reading_options_file_stdin(!IO) :-
    io__write_string("Reading options file from stdin...", !IO).

:- pred write_reading_options_file(string::in, io::di, io::uo) is det.

write_reading_options_file(FileName, !IO) :-
    io__write_string("Reading options file ", !IO),
    io__write_string(FileName, !IO),
    io__nl(!IO).

:- pred write_done(io::di, io::uo) is det.

write_done(!IO) :-
    io__write_string("done.\n", !IO).

:- func maybe_add_path_name(dir_name, file_name) = file_name.

maybe_add_path_name(Dir, File) =
    ( Dir = dir__this_directory ->
        File
    ;
        dir__make_path_name(Dir, File)
    ).

:- pred read_options_lines(dir_name::in, options_variables::in,
    options_variables::out, io::di, io::uo) is det.

read_options_lines(Dir, !Variables, !IO) :-
    io__get_line_number(LineNumber, !IO),
    promise_equivalent_solutions [LineResult, !:IO] (
        read_options_lines_2(Dir, !.Variables, LineResult, !IO)
    ),
    (
        LineResult = succeeded(!:Variables - FoundEOF),
        (
            FoundEOF = yes
        ;
            FoundEOF = no,
            read_options_lines(Dir, !Variables, !IO)
        )
    ;
        LineResult = exception(Exception),
        ( Exception = univ(options_file_error(Error)) ->
            io__input_stream_name(FileName, !IO),
            prog_out__write_context(term__context_init(FileName, LineNumber),
                !IO),
            io__write_string(Error, !IO),
            io__nl(!IO),

            % This will be caught by `read_options_files'.
            % The open options files aren't closed on
            % the way up, but we'll be exiting straight
            % away so that doesn't matter.
            throw(found_options_file_error)
        ;
            rethrow(LineResult)
        )
    ;
        LineResult = failed,
        error("read_options_lines")
    ).

:- pred read_options_lines_2(dir_name::in, options_variables::in,
    exception_result(pair(options_variables, bool))::out,
    io::di, io::uo) is cc_multi.

read_options_lines_2(Dir, Variables0, Result, !IO) :-
    try_io(read_options_lines_3(Dir, Variables0), Result, !IO).

:- pred read_options_lines_3(dir_name::in, options_variables::in,
    pair(options_variables, bool)::out, io::di, io::uo) is det.

read_options_lines_3(Dir, !.Variables, !:Variables - FoundEOF, !IO) :-
    read_options_line(FoundEOF, Line0, !IO),
    (
        Line0 = []
    ;
        Line0 = [_ | _],
        parse_options_line(Line0, ParsedLine),
        (
            ParsedLine = define_variable(VarName, AddToValue, Value),
            update_variable(VarName, AddToValue, Value, !Variables, !IO)
        ;
            ParsedLine = include_options_files(ErrorIfNotExist,
                IncludedFilesChars0),
            expand_variables(!.Variables,
                IncludedFilesChars0, IncludedFilesChars, UndefVars, !IO),
            report_undefined_variables(UndefVars, !IO),
            IncludedFileNames = split_into_words(IncludedFilesChars),
            list__foldl2(
                read_options_file_params(ErrorIfNotExist, search, yes(Dir)),
                IncludedFileNames, !Variables, !IO)
        )
    ).

:- pred read_options_line(bool::out, list(char)::out, io::di, io::uo) is det.

read_options_line(FoundEOF, list__reverse(RevChars), !IO) :-
    io__ignore_whitespace(SpaceResult, !IO),
    ( SpaceResult = error(Error) ->
        throw(options_file_error(io__error_message(Error)))
    ;
        true
    ),
    read_options_line_2(FoundEOF, [], RevChars, !IO).

:- pred read_options_line_2(bool::out, list(char)::in, list(char)::out,
    io::di, io::uo) is det.

read_options_line_2(FoundEOF, !Chars, !IO) :-
    read_item_or_eof(io__read_char, MaybeChar, !IO),
    (
        MaybeChar = yes(Char),
        ( Char = '#' ->
            skip_comment_line(FoundEOF, !IO)
        ; Char = ('\\') ->
            read_item_or_eof(io__read_char, MaybeChar2, !IO),
            (
                MaybeChar2 = yes(Char2),
                ( Char2 = '\n' ->
                    !:Chars = [' ' | !.Chars]
                ;
                    !:Chars = [Char2, Char | !.Chars]
                ),
                read_options_line_2(FoundEOF, !Chars, !IO)
            ;
                MaybeChar2 = no,
                FoundEOF = yes,
                !:Chars = [Char | !.Chars]
            )
        ; Char = '\n' ->
            FoundEOF = no
        ;
            !:Chars = [Char | !.Chars],
            read_options_line_2(FoundEOF, !Chars, !IO)
        )
    ;
        MaybeChar = no,
        FoundEOF = yes
    ).

:- pred update_variable(options_variable::in, bool::in, list(char)::in,
    options_variables::in, options_variables::out, io::di, io::uo) is det.

update_variable(VarName, AddToValue, NewValue0, !Variables, !IO) :-
    expand_variables(!.Variables, NewValue0, NewValue1, Undef, !IO),
    report_undefined_variables(Undef, !IO),
    Words1 = split_into_words(NewValue1),
    io__get_environment_var(VarName, MaybeEnvValue, !IO),
    (
        MaybeEnvValue = yes(EnvValue)
    ->
        Value = string__to_char_list(EnvValue),
        Words = split_into_words(Value),
        OptVarValue = options_variable_value(string__to_char_list(EnvValue),
            Words, environment),
        map__set(!.Variables, VarName, OptVarValue, !:Variables)
    ;
        map__search(!.Variables, VarName,
            options_variable_value(OldValue, OldWords, Source))
    ->
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
            map__set(!.Variables, VarName, OptVarValue, !:Variables)
        )
    ;
        OptVarValue = options_variable_value(NewValue1, Words1, options_file),
        map__set(!.Variables, VarName, OptVarValue, !:Variables)
    ).

:- pred expand_variables(options_variables::in, list(char)::in,
    list(char)::out, list(string)::out, io::di, io::uo) is det.

expand_variables(Variables, Chars0, Chars, UndefVars, !IO) :-
    expand_variables_2(Variables, Chars0, [], RevChars, [], RevUndefVars, !IO),
    list__reverse(RevChars, Chars),
    list__reverse(RevUndefVars, UndefVars).

:- pred expand_variables_2(options_variables::in, list(char)::in,
    list(char)::in, list(char)::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

expand_variables_2(_, [], !RevChars, !RevUndef, !IO).
expand_variables_2(Variables, [Char | Chars], !RevChars, !RevUndef, !IO) :-
    ( Char = '$' ->
        (
            Chars = [],
            throw(options_file_error("unterminated variable reference"))
        ;
            Chars = [Char2 | Chars1],
            ( Char2 = '$' ->
                !:RevChars = ['$' | !.RevChars],
                expand_variables_2(Variables, Chars1, !RevChars, !RevUndef,
                    !IO)
            ;
                (
                    (
                        Char2 = '(',
                        EndChar = ')'
                    ;
                        Char2 = '{',
                        EndChar = '}'
                    )
                ->
                    parse_variable(VarName0, Chars1, Chars2),
                    ( Chars2 = [EndChar | Chars3] ->
                        Chars4 = Chars3,
                        VarName = VarName0
                    ;
                        throw(options_file_error(
                            "unterminated variable reference"))
                    )
                ;
                    Chars4 = Chars1,
                    VarName = string__char_to_string(Char2)
                ),
                lookup_variable_chars(Variables, VarName, VarChars, !RevUndef,
                    !IO),
                !:RevChars = reverse(VarChars) ++ !.RevChars,
                expand_variables_2(Variables, Chars4, !RevChars, !RevUndef,
                    !IO)
            )
        )
    ;
        !:RevChars = [Char | !.RevChars],
        expand_variables_2(Variables, Chars, !RevChars, !RevUndef, !IO)
    ).

:- pred report_undefined_variables(list(string)::in, io::di, io::uo) is det.

report_undefined_variables(Vars, !IO) :-
    report_undefined_variables_2(list__sort_and_remove_dups(Vars), !IO).

:- pred report_undefined_variables_2(list(string)::in, io::di, io::uo) is det.

report_undefined_variables_2([], !IO).
report_undefined_variables_2([_ | Rest] @ UndefVars, !IO) :-
    globals__io_lookup_bool_option(warn_undefined_options_variables, Warn,
        !IO),
    (
        Warn = yes,
        io__input_stream_name(FileName, !IO),
        io__get_line_number(LineNumber, !IO),
        Context = term__context_init(FileName, LineNumber),

        VarList = list_to_pieces(list__map(add_quotes,
            list__sort_and_remove_dups(UndefVars))),
        ( Rest = [], Word = "variable", IsOrAre = "is"
        ; Rest = [_ | _], Word = "variables", IsOrAre = "are"
        ),
        Pieces = [words("Warning: "), words(Word) | VarList]
            ++ [words(IsOrAre), words("undefined.")],
        write_error_pieces(Context, 0, Pieces, !IO),

        globals__io_lookup_bool_option(halt_at_warn, Halt, !IO),
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
    (
        ( Line0 = [('-') | Line1] ->
            ErrorIfNotExist = no_error,
            Line2 = Line1
        ;
            ErrorIfNotExist = error,
            Line2 = Line0
        ),
        list__append(string__to_char_list("include"), Line3, Line2)
    ->
        list__takewhile(char__is_whitespace, Line3, _, Line4),
        OptionsFileLine = include_options_files(ErrorIfNotExist, Line4)
    ;
        parse_variable(VarName, Line0, Line1),
        list__takewhile(char__is_whitespace, Line1, _, Line2),
        ( Line2 = [('=') | Line3] ->
            Add = no,
            Line4 = Line3
        ; Line2 = [('+'), ('=') | Line3] ->
            Add = yes,
            Line4 = Line3
        ; Line2 = [(':'), ('=') | Line3] ->
            Add = no,
            Line4 = Line3
        ;
            throw(options_file_error(
                "expected `=', `:=' or `+=' after `" ++ VarName ++ "'"))
        ),
        list__takewhile(char__is_whitespace, Line4, _, VarValue),
        OptionsFileLine = define_variable(VarName, Add, VarValue)
    ).

:- pred parse_file_name(file_name::out,
    list(char)::in, list(char)::out) is det.

parse_file_name(FileName, Chars0, Chars) :-
    ( Chars0 = ['"' | Chars1] ->
        parse_string(FileName, Chars1, Chars)
    ;
        list__takewhile(isnt(char__is_whitespace), Chars0,
            FileNameChars, Chars),
        FileName = string__from_char_list(FileNameChars)
    ).

:- pred parse_variable(options_variable::out,
    list(char)::in, list(char)::out) is det.

parse_variable(VarName, Chars0, Chars) :-
    parse_variable_2(yes, [], VarList, Chars0, Chars),
    string__from_rev_char_list(VarList, VarName),
    ( VarName = "" ->
        list__takewhile(isnt(char__is_whitespace), Chars, FirstWord, _),
        throw(options_file_error("expected variable at `" ++
            string__from_char_list(FirstWord) ++ "'"))
    ;
        true
    ).

:- pred parse_variable_2(bool::in, list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

parse_variable_2(_, Var, Var, [], []).
parse_variable_2(IsFirst, Var0, Var, [Char | Chars0], Chars) :-
    (
        \+ char__is_whitespace(Char),
        (
            IsFirst = yes,
            char__is_alpha(Char)
        ;
            IsFirst = no,
            ( char__is_alnum_or_underscore(Char)
            ; Char = ('-')
            ; Char = ('.')
            )
        )
    ->
        parse_variable_2(no, [Char | Var0], Var, Chars0, Chars)
    ;
        Var = Var0,
        Chars = [Char | Chars0]
    ).

:- pred parse_string(string::out, list(char)::in, list(char)::out) is det.

parse_string(String, Chars0, Chars) :-
    parse_string_chars([], StringChars, Chars0, Chars),
    String = string__from_rev_char_list(StringChars).

:- pred parse_string_chars(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

parse_string_chars(_, _, [], _) :-
    throw(options_file_error("unterminated string")).
parse_string_chars(String0, String, [Char | Chars0], Chars) :-
    ( Char = '"' ->
        Chars = Chars0,
        String = String0
    ; Char = ('\\') ->
        (
            Chars0 = [Char2 | Chars1],
            ( Char2 = '"' ->
                String1 = [Char2 | String0]
            ;
                String1 = [Char2, Char | String0]
            ),
            parse_string_chars(String1, String, Chars1, Chars)
        ;
            Chars0 = [],
            throw(options_file_error("unterminated string"))
        )
    ;
        parse_string_chars([Char | String0], String, Chars0, Chars)
    ).

:- pred skip_comment_line(bool::out, io::di, io::uo) is det.

skip_comment_line(FoundEOF, !IO) :-
    read_item_or_eof(io__read_char, MaybeChar, !IO),
    (
        MaybeChar = yes(Char),
        ( Char = '\n' ->
            FoundEOF = no
        ;
            skip_comment_line(FoundEOF, !IO)
        )
    ;
        MaybeChar = no,
        FoundEOF = yes
    ).

:- pred read_item_or_eof(
    pred(io__result(T), io, io)::(pred(out, di, uo) is det),
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
        throw(options_file_error(io__error_message(Error)))
    ).

%-----------------------------------------------------------------------------%

:- func checked_split_into_words(list(char)) = maybe_error(list(string)).

checked_split_into_words(Chars) = Result :-
    promise_equivalent_solutions [TryResult] (
        try(
            (pred(Words0::out) is det :-
                Words0 = split_into_words(Chars)
            ), TryResult)
    ),
    (
        TryResult = succeeded(Words),
        Result = ok(Words)
    ;
        TryResult = exception(Exception),
        ( Exception = univ(options_file_error(Msg)) ->
            Result = error(Msg)
        ;
            rethrow(TryResult)
        )
    ).

:- func split_into_words(list(char)) = list(string).

split_into_words(Chars) = list__reverse(split_into_words_2(Chars, [])).

:- func split_into_words_2(list(char), list(string)) = list(string).

split_into_words_2(Chars0, Words0) = Words :-
    list__takewhile(char__is_whitespace, Chars0, _, Chars1),
    (
        Chars1 = [],
        Words = Words0
    ;
        Chars1 = [_ | _],
        get_word(Word, Chars1, Chars),
        Words = split_into_words_2(Chars, [Word | Words0])
    ).

:- pred get_word(string::out, list(char)::in, list(char)::out) is det.

get_word(string__from_rev_char_list(RevWord), Chars0, Chars) :-
    get_word_2([], RevWord, Chars0, Chars).

:- pred get_word_2(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

get_word_2(RevWord, RevWord, [], []).
get_word_2(RevWord0, RevWord, [Char | Chars0], Chars) :-
    ( char__is_whitespace(Char) ->
        Chars = Chars0,
        RevWord = RevWord0
    ; Char = '"' ->
        parse_string_chars([], RevStringChars, Chars0, Chars1),
        get_word_2(RevStringChars ++ RevWord0, RevWord,
            Chars1, Chars)
    ; Char = ('\\') ->
        (
            Chars0 = [],
            RevWord = [Char | RevWord0],
            Chars = []
        ;
            Chars0 = [Char2 | Chars1],
            (
                ( Char2 = '"'
                ; Char2 = ('\\')
                )
            ->
                get_word_2([Char2 | RevWord0], RevWord, Chars1, Chars)
            ;
                get_word_2([Char2, Char | RevWord0], RevWord, Chars1, Chars)
            )
        )
    ;
        get_word_2([Char | RevWord0], RevWord, Chars0, Chars)
    ).

%-----------------------------------------------------------------------------%

lookup_main_target(Vars, MaybeMainTarget, !IO) :-
    lookup_variable_words_report_error(Vars, "MAIN_TARGET", MainTargetResult,
        !IO),
    (
        MainTargetResult = set(MainTarget),
        MaybeMainTarget = yes(MainTarget)
    ;
        MainTargetResult = unset,
        MaybeMainTarget = yes([])
    ;
        MainTargetResult = error(_),
        MaybeMainTarget = no
    ).

lookup_default_options(Vars, Result, !IO) :-
    lookup_mmc_maybe_module_options(Vars, default, Result, !IO).

lookup_mmc_options(Vars, Result, !IO) :-
    lookup_mmc_maybe_module_options(Vars, non_module_specific, Result, !IO).

lookup_mmc_module_options(Vars, ModuleName, Result, !IO) :-
    lookup_mmc_maybe_module_options(Vars, module_specific(ModuleName), Result,
        !IO).

:- pred lookup_mmc_maybe_module_options(options_variables::in,
    options_variable_class::in, maybe(list(string))::out, io::di, io::uo)
    is det.

lookup_mmc_maybe_module_options(Vars, MaybeModuleName, Result, !IO) :-
    VariableTypes = options_variable_types,
    list__map_foldl(lookup_options_variable(Vars, MaybeModuleName),
        VariableTypes, Results, !IO),
    (
        list__map(
            (pred(VarResult::in, MaybeValue::out) is semidet :-
                (
                    VarResult = set(Value),
                    MaybeValue = yes(Value)
                ;
                    VarResult = unset,
                    MaybeValue = no
                )
            ), Results, Values)
    ->
        assoc_list__from_corresponding_lists(VariableTypes,
            Values, VariableValues),
        % Default to `-O2', even when mercury_compile is called directly,
        % not by the mmc script.
        Result = yes(["-O2" | list__condense(
            list__map(convert_to_mmc_options, VariableValues))])
    ;
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
    ;       java_flags
    ;       ilasm_flags
    ;       csharp_flags
    ;       mcpp_flags
    ;       ml_objs
    ;       ml_libs
    ;       ld_flags
    ;       ld_libflags
    ;       c2init_args
    ;       libraries
    ;       lib_dirs
    ;       lib_grades
    ;       install_prefix
    ;       stdlib_dir
    ;       config_dir
    ;       linkage
    ;       mercury_linkage.

:- func options_variable_types = list(options_variable_type).

    % `LIBRARIES' should come before `MLLIBS' (Mercury libraries depend on
    % C libraries, but C libraries typically do not depend on Mercury
    % libraries).
    % `MERCURY_STDLIB_DIR' and `MERCURY_CONFIG_DIR' should come before
    % `MCFLAGS'. Settings in `MCFLAGS' (e.g. `--no-mercury-stdlib-dir')
    % should override settings of these MERCURY_STDLIB_DIR in the environment.
options_variable_types =
    [grade_flags, linkage, mercury_linkage, lib_grades, stdlib_dir,
    config_dir, mmc_flags, c_flags, java_flags, ilasm_flags,
    csharp_flags, mcpp_flags, ml_objs, lib_dirs, ld_flags,
    libraries, ml_libs, c2init_args, install_prefix].

:- func options_variable_name(options_variable_type) = string.

options_variable_name(grade_flags) = "GRADEFLAGS".
options_variable_name(mmc_flags) = "MCFLAGS".
options_variable_name(c_flags) = "CFLAGS".
options_variable_name(java_flags) = "JAVACFLAGS".
options_variable_name(ilasm_flags) = "MS_ILASM_FLAGS".
options_variable_name(mcpp_flags) = "MS_CL_FLAGS".
options_variable_name(csharp_flags) = "MS_CSC_FLAGS".
options_variable_name(ml_objs) = "MLOBJS".
options_variable_name(ml_libs) = "MLLIBS".
options_variable_name(ld_flags) = "LDFLAGS".
options_variable_name(ld_libflags) = "LD_LIBFLAGS".
options_variable_name(c2init_args) = "C2INITARGS".
options_variable_name(libraries) = "LIBRARIES".
options_variable_name(lib_dirs) = "LIB_DIRS".
options_variable_name(lib_grades) = "LIBGRADES".
options_variable_name(install_prefix) = "INSTALL_PREFIX".
options_variable_name(stdlib_dir) = "MERCURY_STDLIB_DIR".
options_variable_name(config_dir) = "MERCURY_CONFIG_DIR".
options_variable_name(linkage) = "LINKAGE".
options_variable_name(mercury_linkage) = "MERCURY_LINKAGE".

:- func options_variable_type_is_target_specific(options_variable_type) = bool.

options_variable_type_is_target_specific(grade_flags) = no.
options_variable_type_is_target_specific(mmc_flags) = yes.
options_variable_type_is_target_specific(c_flags) = yes.
options_variable_type_is_target_specific(java_flags) = yes.
options_variable_type_is_target_specific(ilasm_flags) = yes.
options_variable_type_is_target_specific(mcpp_flags) = yes.
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
options_variable_type_is_target_specific(linkage) = yes.
options_variable_type_is_target_specific(mercury_linkage) = yes.

:- func convert_to_mmc_options(
    pair(options_variable_type, maybe(list(string)))) = list(string).

convert_to_mmc_options(_ - no) = [].
convert_to_mmc_options(VariableType - yes(VariableValue)) =
    convert_to_mmc_options(VariableType, VariableValue).

:- func convert_to_mmc_options(options_variable_type, list(string))
    = list(string).

convert_to_mmc_options(VariableType, VariableValue) = OptionsStrings :-
    MMCOptionType = mmc_option_type(VariableType),
    (
        MMCOptionType = mmc_flags,
        OptionsStrings = VariableValue
    ;
        MMCOptionType = option(InitialOptions, OptionName),
        OptionsStrings = list__condense([InitialOptions |
            list__map((func(Word) = [OptionName, Word]), VariableValue)])
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
mmc_option_type(java_flags) = option([], "--java-flag").
mmc_option_type(ilasm_flags) = option([], "--ilasm-flag").
mmc_option_type(mcpp_flags) = option([], "--mcpp-flag").
mmc_option_type(csharp_flags) = option([], "--csharp-flag").
mmc_option_type(ml_objs) = option([], "--link-object").
mmc_option_type(ml_libs) = mmc_flags.
mmc_option_type(ld_flags) = option([], "--ld-flag").
mmc_option_type(ld_libflags) = option([], "--ld-libflag").
mmc_option_type(c2init_args) = option([], "--init-file").
mmc_option_type(libraries) = option([], "--mercury-library").
mmc_option_type(lib_dirs) = option([], "--mercury-library-directory").
mmc_option_type(lib_grades) = option(["--no-libgrade"], "--libgrade").
mmc_option_type(install_prefix) = option([], "--install-prefix").
mmc_option_type(stdlib_dir) = option([], "--mercury-stdlib-dir").
mmc_option_type(config_dir) = option([], "--mercury-config-dir").
mmc_option_type(linkage) = option([], "--linkage").
mmc_option_type(mercury_linkage) = option([], "--mercury-linkage").

%-----------------------------------------------------------------------------%

:- type variable_result(T)
    --->    set(T)
    ;       unset
    ;       error(string).

:- pred lookup_options_variable(options_variables::in,
    options_variable_class::in, options_variable_type::in,
    variable_result(list(string))::out,
    io::di, io::uo) is det.

lookup_options_variable(Vars, OptionsVariableClass, FlagsVar, Result, !IO) :-
    VarName = options_variable_name(FlagsVar),
    lookup_variable_words_report_error(Vars, "DEFAULT_" ++ VarName,
        DefaultFlagsResult, !IO),
    ( OptionsVariableClass = default ->
        FlagsResult = unset,
        ExtraFlagsResult = unset
    ;
        lookup_variable_words_report_error(Vars, VarName, FlagsResult, !IO),
        lookup_variable_words_report_error(Vars, "EXTRA_" ++ VarName,
            ExtraFlagsResult, !IO)
    ),
    (
        OptionsVariableClass = module_specific(ModuleName),
        options_variable_type_is_target_specific(FlagsVar) = yes
    ->
        sym_name_to_string(ModuleName, ".", ModuleFileNameBase),
        ModuleVarName = VarName ++ "-" ++ ModuleFileNameBase,
        lookup_variable_words_report_error(Vars, ModuleVarName,
            ModuleFlagsResult, !IO)
    ;
        ModuleFlagsResult = unset
    ),
    Result = DefaultFlagsResult ++ FlagsResult ++ ExtraFlagsResult
        ++ ModuleFlagsResult.

:- func variable_result(list(T)) ++ variable_result(list(T)) =
    variable_result(list(T)).

unset ++ unset = unset.
unset ++ set(V) = set(V).
unset ++ error(E) = error(E).
set(V1) ++ set(V2) = set(V1 ++ V2).
set(V) ++ unset = set(V).
set(_) ++ error(E) = error(E).
error(E) ++ _ = error(E).

:- pred lookup_variable_words_report_error(options_variables::in,
    options_variable::in, variable_result(list(string))::out,
    io::di, io::uo) is det.

lookup_variable_words_report_error(Vars, VarName, Result, !IO) :-
    lookup_variable_words(Vars, VarName, Result, !IO),
    ( Result = error(Error) ->
        io__write_string(Error, !IO),
        io__nl(!IO)
    ;
        true
    ).

:- pred lookup_variable_words(options_variables::in, options_variable::in,
    variable_result(list(string))::out, io::di, io::uo) is det.

lookup_variable_words(Vars, VarName, Result) -->
    lookup_variable_words(yes, Vars, VarName, Result).

:- pred lookup_variable_words(bool::in, options_variables::in,
    options_variable::in, variable_result(list(string))::out,
    io::di, io::uo) is det.

lookup_variable_words(LookupEnv, Vars, VarName, Result, !IO) :-
    (
        LookupEnv = yes,
        io__get_environment_var(VarName, MaybeEnvValue, !IO)
    ;
        LookupEnv = no,
        MaybeEnvValue = no
    ),
    ( MaybeEnvValue = yes(EnvValue) ->
        SplitResult = checked_split_into_words(string__to_char_list(EnvValue)),
        (
            SplitResult = ok(EnvWords),
            Result = set(EnvWords)
        ;
            SplitResult = error(Msg),
            Result = error("Error: in environment variable `"
                ++ VarName ++ "': " ++ Msg)
        )
    ; map__search(Vars, VarName, MapValue) ->
        MapValue = options_variable_value(_, Words, _),
        Result = set(Words)
    ;
        Result = unset
    ).

:- pred lookup_variable_chars(options_variables::in, string::in,
    list(char)::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

lookup_variable_chars(Variables, Var, Value, !Undef, !IO) :-
    io__get_environment_var(Var, MaybeValue, !IO),
    (
        MaybeValue = yes(ValueString),
        Value = string__to_char_list(ValueString)
    ;
        MaybeValue = no,
        ( map__search(Variables, Var, options_variable_value(Value0, _, _)) ->
            Value = Value0
        ;
            Value = [],
            !:Undef = [Var | !.Undef]
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "options_file.m.".

%-----------------------------------------------------------------------------%
:- end_module options_file.
%-----------------------------------------------------------------------------%
