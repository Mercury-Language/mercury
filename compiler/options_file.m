%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: options_file.m
% Main author: stayl
%
% Code to deal with options for `mmc --make', including code to parse
% an Mmakefile equivalent.
%-----------------------------------------------------------------------------%
:- module make__options_file.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module list, io, std_util.

:- type options_variables.

:- func options_variables_init = options_variables.

:- pred read_options_files(maybe(options_variables)::out,
		io__state::di, io__state::uo) is det.

	% Look up the DEFAULT_MCFLAGS variable.
:- pred lookup_default_options(options_variables::in, maybe(list(string))::out,
	io__state::di, io__state::uo) is det.

	% Look up all the non-module specific options.
:- pred lookup_mmc_options(options_variables::in, maybe(list(string))::out,
	io__state::di, io__state::uo) is det.

	% Same as lookup_mmc_module_options, but also adds the
	% module-specific (MCFLAGS-module) options.
:- pred lookup_mmc_module_options(options_variables::in, module_name::in,
	maybe(list(string))::out, io__state::di, io__state::uo) is det.

	% Look up $(MAIN_TARGET).
:- pred lookup_main_target(options_variables::in, maybe(list(string))::out,
	io__state::di, io__state::uo) is det.

	% Quote any strings containing whitespace.
:- func quote_args(list(string)) = list(string).

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module parse_tree__prog_io, parse_tree__prog_out.
:- import_module hlds__error_util, libs__globals, libs__options.

:- import_module assoc_list, bool, char, dir, exception, map.
:- import_module require, string, term.

:- type options_variable == string.

:- type options_file_error
	--->	options_file_error(string).

:- type found_options_file_error
	--->	found_options_file_error.

:- type options_variables == map(options_variable, options_variable_value).

:- type options_variable_value
	---> 	options_variable_value(
			list(char),
			list(string),	% split into words.
			variable_source
		).
		
:- type variable_source
	--->	options_file
	;	command_line
	;	environment
	.

options_variables_init = map__init.

read_options_files(MaybeVariables) -->
	promise_only_solution_io(
	    (pred(R::out, di, uo) is cc_multi -->
		try_io(
	    	    (pred((Variables1)::out, di, uo) is det -->
			globals__io_lookup_accumulating_option(options_files,
				OptionsFiles),
			{ Variables0 = options_variables_init },
			{ ReadFile =
			    (pred(OptionsFile::in, Vars0::in, Vars::out,
					di, uo) is det -->
				{ OptionsFile = "Mercury.options" ->
					ErrorIfNotExist = error,
					Search = no_search
				;
					ErrorIfNotExist = no_error,
					Search = search
				},
				read_options_file(ErrorIfNotExist, Search, no,
					OptionsFile, Vars0, Vars)
			    ) },
			list__foldl2(ReadFile, OptionsFiles,
				Variables0, Variables1)
		    ), R)
	    ), OptionsFileResult),
	(
		{ OptionsFileResult = succeeded(Variables) },
		{ MaybeVariables = yes(Variables) }
	;
		{ OptionsFileResult = exception(Exception) },
		{ Exception = univ(found_options_file_error) ->
			MaybeVariables = no
		;
			rethrow(OptionsFileResult)
		}
	;
		{ OptionsFileResult = failed },
		{ error("read_options_files") }
	).

:- type error_if_not_exist
	--->	error
	;	no_error.

:- type search
	--->	search
	;	no_search.

	% read_options_file(ErrorIfNotExist, Search, MaybeDirName,
	% 	FileName, Variables0, Variables).
:- pred read_options_file(error_if_not_exist::in, search::in,
		maybe(dir_name)::in, string::in, options_variables::in,
		options_variables::out, io__state::di, io__state::uo) is det.

read_options_file(ErrorIfNotExist0, Search, MaybeDirName, OptionsFile0,
		Variables0, Variables) -->
    ( { OptionsFile0 = "-" } ->
	% Read from standard input.
	read_options_lines(dir__this_directory, Variables0, Variables)
    ;
	( { OptionsFile0 = "Mercury.options" } ->
		% Don't complain if the "Mercury.options"
		% file doesn't exist.
		{ ErrorIfNotExist = no_error },
		{ SearchDirs = [dir__this_directory] }
	; { Search = search } ->
		{ ErrorIfNotExist = ErrorIfNotExist0 },
		globals__io_lookup_accumulating_option(
			options_search_directories, SearchDirs)
	;
		{ ErrorIfNotExist = ErrorIfNotExist0 },
		{ SearchDirs = [dir__this_directory] }
	),
	{ dir__split_name(OptionsFile0, OptionsDir, OptionsFile) },
	(
		% Is it an absolute pathname?
		% XXX This won't work on Windows
		% (but GNU Make does it this way too).
		{ string__index(OptionsDir, 0,
			dir__directory_separator) }
	->
		{ FileToFind = OptionsFile },
		{ Dirs = [OptionsDir] }
	;
		{ MaybeDirName = yes(DirName) }
	->
		{ FileToFind = OptionsFile },
		{ Dirs = [dir__make_path_name(DirName, OptionsDir)
				| SearchDirs] }
	;
		{ Dirs = SearchDirs },
		{ FileToFind = OptionsFile0 }
	),
	io__input_stream(OldInputStream),
	search_for_file_returning_dir(Dirs, FileToFind, MaybeDir),
	(
		{ MaybeDir = ok(FoundDir) },
		read_options_lines(FoundDir, Variables0, Variables),
		io__input_stream(OptionsStream),
		io__set_input_stream(OldInputStream, _),
		io__close_input(OptionsStream)
	;
		{ MaybeDir = error(_) },
		{ Variables = Variables0 },
		( { ErrorIfNotExist = error } ->
			{ Dirs = [SingleDir] ->
				ErrorFile = maybe_add_path_name(SingleDir,
						OptionsFile)	
			;
				ErrorFile = OptionsFile
			},
			io__write_string("Error reading options file `"),
			io__write_string(ErrorFile),
			io__write_string("'.\n"),
			io__set_exit_status(1)
		;
			[]
		)
	)
    ).

:- func maybe_add_path_name(dir_name, file_name) = file_name.

maybe_add_path_name(Dir, File) =
	( Dir = dir__this_directory -> File ; dir__make_path_name(Dir, File) ).

:- pred read_options_lines(dir_name::in, options_variables::in,
		options_variables::out, io__state::di, io__state::uo) is det.

read_options_lines(Dir, Variables0, Variables) -->
	io__get_line_number(LineNumber),
	promise_only_solution_io(
	    (pred(R::out, di, uo) is cc_multi -->
		try_io(
	    	    (pred((Variables1 - FoundEOF1)::out, di, uo) is det -->
			read_options_line(FoundEOF1, [], Line0),
			(
			    { Line0 = [] },
			    { Variables1 = Variables0 }	
			;
			    { Line0 = [_|_] },
			    { parse_options_line(Line0, ParsedLine) },
			    (
				{ ParsedLine = define_variable(VarName,
						AddToValue, Value) },
				update_variable(VarName, AddToValue, Value,
						Variables0, Variables1)
			    ;
				{ ParsedLine = include_options_files(
						ErrorIfNotExist,
						IncludedFilesChars0) },
				expand_variables(Variables0,
					IncludedFilesChars0,
					IncludedFilesChars, UndefVars),
				report_undefined_variables(UndefVars),
				{ IncludedFileNames =
					split_into_words(IncludedFilesChars) },
				list__foldl2(
					read_options_file(ErrorIfNotExist,
						search, yes(Dir)),
					IncludedFileNames,
					Variables0, Variables1)
			    )
		        )
		    ), R)
	    ), LineResult),
	(
		{ LineResult = succeeded(Variables2 - FoundEOF) },
		(
			{ FoundEOF = yes },
			{ Variables = Variables2 }
		;
			{ FoundEOF = no },
			read_options_lines(Dir, Variables2, Variables)
		)
	;
		{ LineResult = exception(Exception) },
		( { Exception = univ(options_file_error(Error)) } ->
			{ Variables = Variables0 },
			io__input_stream_name(FileName),
			prog_out__write_context(
				term__context_init(FileName, LineNumber)),
			io__write_string(Error),
			io__nl,

			% This will be caught by `read_options_files'.
			% The open options files aren't closed on
			% the way up, but we'll be exiting straight
			% away so that doesn't matter.
			{ throw(found_options_file_error) }
		;
			{ rethrow(LineResult) }
		)
	;
		{ LineResult = failed },
		{ error("read_options_lines") }
	).

:- pred read_options_line(bool::out, list(char)::in, list(char)::out,
		io__state::di, io__state::uo) is det.

read_options_line(FoundEOF, Chars0, list__reverse(RevChars)) -->
	io__ignore_whitespace(SpaceResult),
	{ SpaceResult = error(Error) ->
		throw(options_file_error(io__error_message(Error)))
	;
		true
	},
	read_options_line_2(FoundEOF, Chars0, RevChars).

:- pred read_options_line_2(bool::out, list(char)::in, list(char)::out,
		io__state::di, io__state::uo) is det.

read_options_line_2(FoundEOF, Chars0, Chars) -->
    read_item_or_eof(io__read_char, MaybeChar),
    (
	{ MaybeChar = yes(Char) },
	( { Char = '#' } ->
		skip_comment_line(FoundEOF),
		{ Chars = Chars0 }
	; { Char = ('\\') } ->
    	    read_item_or_eof(io__read_char, MaybeChar2),
	    (
		{ MaybeChar2 = yes(Char2) },
		( { Char2 = '\n' } ->
		    read_options_line_2(FoundEOF, ['\n' | Chars0], Chars)
		;
		    read_options_line_2(FoundEOF,
		    		[Char2, Char | Chars0], Chars)
		)
	    ;
		{ MaybeChar2 = no },
		{ FoundEOF = yes },
		{ Chars = [Char | Chars0] }
	    )
    	; { Char = '\n' } ->
    	    { FoundEOF = no },
	    { Chars = Chars0 }
	;
	    read_options_line_2(FoundEOF, [Char | Chars0], Chars)
	)
    ;
	{ MaybeChar = no },
	{ FoundEOF = yes },
	{ Chars = Chars0 }
    ).

:- pred update_variable(options_variable::in, bool::in, list(char)::in,
		options_variables::in, options_variables::out,
		io__state::di, io__state::uo) is det.

update_variable(VarName, AddToValue, NewValue0, Variables0, Variables) -->
	expand_variables(Variables0, NewValue0, NewValue1, Undef),
	report_undefined_variables(Undef),
	{ Words1 = split_into_words(NewValue1) },
	io__get_environment_var(VarName, MaybeEnvValue),
	(
		{ MaybeEnvValue = yes(EnvValue) }
	->
		{ Value = string__to_char_list(EnvValue) },
		{ Words = split_into_words(Value) },
		{ map__set(Variables0, VarName,
			options_variable_value(string__to_char_list(EnvValue),
				Words, environment),
			Variables) }
	;
		{ map__search(Variables0, VarName,
			options_variable_value(OldValue, OldWords, Source)) }
	->
		(
			{ Source = environment },
			{ Variables = Variables0 }
		;
			{ Source = command_line },
			{ Variables = Variables0 }
		;
			{ Source = options_file },
			{ AddToValue = yes ->
				NewValue = OldValue ++ [' ' |  NewValue1],
				Words = OldWords ++ Words1
			;
				NewValue = NewValue1,
				Words = Words1
			},
			{ map__set(Variables0, VarName,
				options_variable_value(NewValue,
					Words, options_file),
				Variables) }
		)
	;
		{ map__set(Variables0, VarName,
			options_variable_value(NewValue1,
				Words1, options_file),
			Variables) }
	).

:- pred expand_variables(options_variables::in, list(char)::in,
	list(char)::out, list(string)::out,
	io__state::di, io__state::uo) is det.

expand_variables(Variables, Chars0, Chars, UndefVars) -->
	expand_variables_2(Variables, Chars0, [], Chars, [], UndefVars).

:- pred expand_variables_2(options_variables::in, list(char)::in,
	list(char)::in, list(char)::out,
	list(string)::in, list(string)::out,
	io__state::di, io__state::uo) is det.

expand_variables_2(_, [], RevChars, list__reverse(RevChars),
		Undef, list__reverse(Undef)) --> [].
expand_variables_2(Variables, [Char | Chars], RevChars0, RevChars,
		Undef0, Undef) -->
	( { Char = '$' } ->
	    (
		{ Chars = [] },
		{ throw(
		options_file_error("unterminated variable reference")) }
	    ;
		{ Chars = [Char2 | Chars1] },
		( { Char2 = '$' } ->
		    expand_variables_2(Variables, Chars1,
			['$' | RevChars0], RevChars, Undef0, Undef)
		;
		    {
			( Char2 = '(', EndChar = ')'
			; Char2 = '{', EndChar = '}'
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
		    },
			
		    lookup_variable_chars(Variables, VarName,
				VarChars, Undef0, Undef1),
		    expand_variables_2(Variables, Chars4,
				reverse(VarChars) ++ RevChars0,
				RevChars, Undef1, Undef)
		)
	    )
	;
	    expand_variables_2(Variables, Chars, [Char | RevChars0],
		RevChars, Undef0, Undef)
	).

:- pred report_undefined_variables(list(string)::in,
		io__state::di, io__state::uo) is det.

report_undefined_variables([]) --> [].
report_undefined_variables([_|Rest] @ UndefVars) -->
	globals__io_lookup_bool_option(warn_undefined_options_variables, Warn),
	( { Warn = yes } ->
		io__input_stream_name(FileName),
		io__get_line_number(LineNumber),
		{ Context = term__context_init(FileName, LineNumber) },

		{ error_util__list_to_pieces(
			list__map((func(Var) = "`" ++ Var ++ "'"), UndefVars),
			VarList) },
		{ Rest = [], Word = "variable"
		; Rest = [_|_], Word = "variables"
		},
		{ Pieces =
			[words("Warning: "), words(Word) | VarList]
			++ [words("are undefined.")] },
		write_error_pieces(Context, 0, Pieces),

		globals__io_lookup_bool_option(halt_at_warn, Halt),
		( { Halt = yes } ->
			{ throw(found_options_file_error) }
		;
			[]
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- type options_file_line
	--->	define_variable(
			options_variable,
			bool,		% Add to any existing value?
			list(char)
		)
	;	include_options_files(
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
		OptionsFileLine = include_options_files(
					ErrorIfNotExist, Line4)
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
				"expected `=', `:=' or `+=' after `"
				++ VarName ++ "'"))
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
		list__takewhile(isnt(char__is_whitespace), Chars,
			FirstWord, _),
		throw(options_file_error(
			string__append_list(["expected variable at `",
				string__from_char_list(FirstWord), "'"])))
	;
		true
	).

:- pred parse_variable_2(bool::in, list(char)::in, list(char)::out,
		list(char)::in, list(char)::out) is det.

parse_variable_2(_, Var, Var, [], []).
parse_variable_2(IsFirst, Var0, Var, [Char | Chars0], Chars) :-
	(
		\+ char__is_whitespace(Char),
		( IsFirst = yes ->
			char__is_alpha(Char) 
		;
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

:- pred skip_comment_line(bool::out, io__state::di, io__state::uo) is det.

skip_comment_line(FoundEOF) -->
	read_item_or_eof(io__read_char, MaybeChar),
	(
		{ MaybeChar = yes(Char) },
		( { Char = '\n' } ->
			{ FoundEOF = no }
		;
			skip_comment_line(FoundEOF)
		)
	;
		{ MaybeChar = no },
		{ FoundEOF = yes }
	).

:- pred read_item_or_eof(
	pred(io__result(T), io__state, io__state)::(pred(out, di, uo) is det),
	maybe(T)::out, io__state::di, io__state::uo) is det.

read_item_or_eof(Pred, MaybeItem) -->
	Pred(Result),
	(
		{ Result = ok(Item) },
		{ MaybeItem = yes(Item) }
	;
		{ Result = eof },
		{ MaybeItem = no }
	;
		{ Result = error(Error) },
		{ throw(options_file_error(io__error_message(Error))) }
	).

%-----------------------------------------------------------------------------%

:- func checked_split_into_words(list(char)) = maybe_error(list(string)).

checked_split_into_words(Chars) = Result :-
	TryResult =
	    promise_only_solution(
		(pred(TResult::out) is cc_multi :-
		    try(
			(pred(Words0::out) is det :-
			    Words0 = split_into_words(Chars)
			), TResult)
		)),
	(
		TryResult = succeeded(Words),
		Result = ok(Words)
	;
		TryResult = failed,
		error("split_into_words failed")
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
		Chars1 = [_|_],
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
			get_word_2([Char2 | RevWord0], RevWord,
				Chars1, Chars)
		)
	;
		get_word_2([Char | RevWord0], RevWord, Chars0, Chars)
	).

%-----------------------------------------------------------------------------%

lookup_main_target(Vars, MaybeMainTarget) -->
	lookup_variable_words_report_error(Vars,
		"MAIN_TARGET", MaybeMainTarget).

lookup_default_options(Vars, Result) -->
	lookup_mmc_maybe_module_options(Vars, default, Result).

lookup_mmc_options(Vars, Result) -->
	lookup_mmc_maybe_module_options(Vars, non_module_specific, Result).

lookup_mmc_module_options(Vars, ModuleName, Result) -->
	lookup_mmc_maybe_module_options(Vars,
		module_specific(ModuleName), Result).

:- pred lookup_mmc_maybe_module_options(options_variables::in,
	options_variable_class::in, maybe(list(string))::out,
	io__state::di, io__state::uo) is det.

lookup_mmc_maybe_module_options(Vars, MaybeModuleName, Result) -->
	{ VariableTypes = options_variable_types },
	list__map_foldl(lookup_options_variable(Vars, MaybeModuleName),
		VariableTypes, Results),
	{
		list__map((pred(yes(Value)::in, Value::out) is semidet),
			Results, Values)
	->
		assoc_list__from_corresponding_lists(VariableTypes,
			Values, VariableValues),
		Result = yes(list__condense(
			list__map(convert_to_mmc_options, VariableValues)))
	;
		Result = no
	}.

:- type options_variable_class
	--->	default
	;	non_module_specific
	;	module_specific(module_name)
	.


:- type options_variable_type
	--->	grade_flags
	;	mmc_flags
	;	c_flags
	;	java_flags
	;	ilasm_flags
	;	csharp_flags
	;	mcpp_flags
	;	ml_flags
	;	ml_objs
	;	ml_libs
	;	ld_flags
	;	ld_libflags
	;	c2init_args
	;	libraries
	;	lib_dirs
	.

:- func options_variable_types = list(options_variable_type).

	% `LIBRARIES' should come before `MLLIBS' (Mercury libraries
	% depend on C libraries, but C libraries typically do not
	% depend on Mercury libraries).
options_variable_types =
	[grade_flags, mmc_flags, c_flags, java_flags,
	ilasm_flags, csharp_flags, mcpp_flags,
	ml_objs, lib_dirs, ml_flags, ld_flags,
	libraries, ml_libs, c2init_args].

:- func options_variable_name(options_variable_type) = string.

options_variable_name(grade_flags) = "GRADEFLAGS".
options_variable_name(mmc_flags) = "MCFLAGS".
options_variable_name(c_flags) = "CFLAGS".
options_variable_name(java_flags) = "JAVACFLAGS".
options_variable_name(ilasm_flags) = "MS_ILASM_FLAGS".
options_variable_name(mcpp_flags) = "MS_CL_FLAGS".
options_variable_name(csharp_flags) = "MS_CSC_FLAGS".
options_variable_name(ml_flags) = "MLFLAGS".
options_variable_name(ml_objs) = "MLOBJS".
options_variable_name(ml_libs) = "MLLIBS".
options_variable_name(ld_flags) = "LDFLAGS".
options_variable_name(ld_libflags) = "LD_LIBFLAGS".
options_variable_name(c2init_args) = "C2INITARGS".
options_variable_name(libraries) = "LIBRARIES".
options_variable_name(lib_dirs) = "LIB_DIRS".

:- func options_variable_type_is_target_specific(options_variable_type) = bool.

options_variable_type_is_target_specific(grade_flags) = no.
options_variable_type_is_target_specific(mmc_flags) = yes.
options_variable_type_is_target_specific(c_flags) = yes.
options_variable_type_is_target_specific(java_flags) = yes.
options_variable_type_is_target_specific(ilasm_flags) = yes.
options_variable_type_is_target_specific(mcpp_flags) = yes.
options_variable_type_is_target_specific(csharp_flags) = yes.
options_variable_type_is_target_specific(ml_flags) = yes.
options_variable_type_is_target_specific(ml_objs) = yes.
options_variable_type_is_target_specific(ml_libs) = yes.
options_variable_type_is_target_specific(ld_flags) = yes.
options_variable_type_is_target_specific(ld_libflags) = yes.
options_variable_type_is_target_specific(c2init_args) = yes.
options_variable_type_is_target_specific(libraries) = yes.
options_variable_type_is_target_specific(lib_dirs) = no.

:- func convert_to_mmc_options(pair(options_variable_type, list(string)))
			= list(string).

convert_to_mmc_options(VariableType - VariableValue) = OptionsStrings :-
	MMCOptionType = mmc_option_type(VariableType),
	(
		MMCOptionType = mmc_flags,
		OptionsStrings = VariableValue
	;
		MMCOptionType = option(not_split, OptionName),
		OptionsStrings = [OptionName,
					string__join_list(" ", VariableValue)]
	;
		MMCOptionType = option(split, OptionName),
		OptionsStrings = list__condense(
				list__map((func(Word) = [OptionName, Word]),
					VariableValue))
	).

:- type mmc_option_type
	--->	mmc_flags	% The options can be passed directly to mmc.

	;	option(split_into_words, option_name :: string)
				% The options need to be passed as an
				% argument of an option to mmc.
	.

	% The split_into_words type specifies whether there should be
	% one mmc option per word in a variable's value, or just a single
	% mmc option.
	% The value of CFLAGS is converted into a single `--cflags' option.
	% The value of MLOBJS is converted into multiple `--link-object'
	% options.
:- type split_into_words
	--->	split
	;	not_split
	.

:- func mmc_option_type(options_variable_type) = mmc_option_type.

mmc_option_type(grade_flags) = mmc_flags.
mmc_option_type(mmc_flags) = mmc_flags.
mmc_option_type(c_flags) = option(not_split, "--cflags").
mmc_option_type(java_flags) = option(not_split, "--java-flags").
mmc_option_type(ilasm_flags) = option(not_split, "--ilasm-flags").
mmc_option_type(mcpp_flags) = option(not_split, "--mcpp-flags").
mmc_option_type(csharp_flags) = option(not_split, "--csharp-flags").
mmc_option_type(ml_flags) = option(not_split, "--link-flags").
mmc_option_type(ml_objs) = option(split, "--link-object").
mmc_option_type(ml_libs) = mmc_flags.
mmc_option_type(ld_flags) = option(not_split, "--ld-flags").
mmc_option_type(ld_libflags) = option(not_split, "--ld-libflags").
mmc_option_type(c2init_args) = option(split, "--init-file").
mmc_option_type(libraries) = option(split, "--mercury-library").
mmc_option_type(lib_dirs) = option(split, "--mercury-library-directory").

%-----------------------------------------------------------------------------%

:- pred lookup_options_variable(options_variables::in,
	options_variable_class::in, options_variable_type::in,
	maybe(list(string))::out, io__state::di, io__state::uo) is det.

lookup_options_variable(Vars, OptionsVariableClass, FlagsVar, Result) -->
	{ VarName = options_variable_name(FlagsVar) },
	lookup_variable_words_report_error(Vars, "DEFAULT_" ++ VarName,
		DefaultFlagsResult),
	(
		{ OptionsVariableClass = default }
	->
		{ FlagsResult = yes([]) },
		{ ExtraFlagsResult = yes([]) }
	;
		lookup_variable_words_report_error(Vars, VarName, FlagsResult),
		lookup_variable_words_report_error(Vars, "EXTRA_" ++ VarName,
			ExtraFlagsResult)
	),
	(
		{ OptionsVariableClass = module_specific(ModuleName) },
		{ options_variable_type_is_target_specific(FlagsVar) = yes }
	->
		{ prog_out__sym_name_to_string(ModuleName,
			".", ModuleFileNameBase) },
		{ ModuleVarName = VarName ++ "-" ++ ModuleFileNameBase },
		lookup_variable_words_report_error(Vars, ModuleVarName,
			ModuleFlagsResult)
	;
		{ ModuleFlagsResult = yes([]) }
	),

	(
		{ DefaultFlagsResult = yes(DefaultFlags) },
		{ FlagsResult = yes(Flags) },
		{ ExtraFlagsResult = yes(ExtraFlags) },
		{ ModuleFlagsResult = yes(TargetFlags) }
	->
		{ Result = yes(list__condense([DefaultFlags,
				Flags, ExtraFlags, TargetFlags])) }
	;
		{ Result = no }
	).

:- pred lookup_variable_words_report_error(options_variables::in,
	options_variable::in, maybe(list(string))::out,
	io__state::di, io__state::uo) is det.

lookup_variable_words_report_error(Vars, VarName, Result) -->
	lookup_variable_words(Vars, VarName, Result0),
	(
		{ Result0 = ok(Words) },
		{ Result = yes(Words) }
	;
		{ Result0 = error(Error) },
		{ Result = no },
		io__write_string(Error),
		io__nl
	).

:- pred lookup_variable_words(options_variables::in, options_variable::in,
	maybe_error(list(string))::out,
	io__state::di, io__state::uo) is det.

lookup_variable_words(Vars, VarName, Result) -->
	io__get_environment_var(VarName, MaybeEnvValue),
	( { MaybeEnvValue = yes(EnvValue) } ->
		{ SplitResult = checked_split_into_words(
			string__to_char_list(EnvValue)) },
		{
			SplitResult = ok(EnvWords),
			Result = ok(EnvWords)
		;
			SplitResult = error(Msg),
			Result = error(string__append_list(
					["Error: in environment variable `",
					VarName, "': ", Msg]))
		}
	; { map__search(Vars, VarName, MapValue) } ->
		{ MapValue = options_variable_value(_, Words, _) },
		{ Result = ok(Words) }
	;
		{ Result = ok([]) }
	).

:- pred lookup_variable_chars(options_variables::in, string::in, list(char)::out,
	list(string)::in, list(string)::out,
	io__state::di, io__state::uo) is det.

lookup_variable_chars(Variables, Var, Value, Undef0, Undef) -->
	io__get_environment_var(Var, MaybeValue),
	{
		MaybeValue = yes(ValueString),
		Value = string__to_char_list(ValueString),
		Undef = Undef0
	;
		MaybeValue = no,
		(
			map__search(Variables, Var,
				options_variable_value(Value0, _, _))
		->
			Value = Value0,
			Undef = Undef0
		;
			Value = [],
			Undef = [Var | Undef0]
		)
	}.

%-----------------------------------------------------------------------------%

quote_args(Args) = list__map(quote_arg, Args).

:- func quote_arg(string) = string.

quote_arg(Arg0) = Arg :-
	ArgList = quote_arg_2(string__to_char_list(Arg0)),
	(
		list__member(Char, ArgList),
		( char__is_whitespace(Char)
		; Char = ('\\')
		; Char = '"'
		)
	->
		Arg = "'" ++ string__from_char_list(ArgList) ++ "'"
	;
		Arg = string__from_char_list(ArgList)
	).

:- func quote_arg_2(list(char)) = list(char).

quote_arg_2([]) = [].
quote_arg_2([Char | Chars0]) = Chars :-
	Chars1 = quote_arg_2(Chars0),
	( Char = ('\\') ->
		Chars = [Char, Char | Chars1]
	; Char = '\n' ->
		Chars = [('\\'), 'n' | Chars1]
	; Char = '"' ->
		Chars = [('\\'), '"' | Chars1]
	;
		Chars = [Char | Chars1]	
	).	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
