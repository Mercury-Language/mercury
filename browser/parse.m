%---------------------------------------------------------------------------%
% Copyright (C) 1998-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: parse.m:
% author: aet

% This file contains the parser for the term browser command language.
% If the term browser is called from mdb, it parses the stuff you type
% at the "browser> " prompt after typing "browse" from the mdb prompt.
% If it is called from the external debugger, then it parses the stuff
% contained in a term `external_request(<string to parse>)' send by the
% external debugger.

%---------------------------------------------------------------------------%

% The Command Language
%
%	commandline:
%		"?"				// SICStus help
%		"^" [path]			// SICStus cd
%		"d"				// SICStus display
%		"w"				// SICStus write
%		"<"				// SICStus set depth
%		"help"
%		"h"				// short for help
%		"cd" [path]
%		"pwd"
%		"ls" [formatoptions] [path]
%		"print" [formatoptions] [path]
%		"p" [formatoptions] [path]	// short for print
%		"display"
%		"write"
%		"set" [[setoptions] varvalue]
%		"track" [--accurate] [path]
%		"t" [--accurate] [path]
%		"mark" [--accurate] [path]
%		"m" [--accurate] [path]
%		"mode" [path]
%		"quit"
%
%	formatoptions:
%		/* empty */
%		formatoption formatoptions
%
%	formatoption:
%		-f
%		-r
%		-v
%		-p
%		--flat
%		--raw-pretty
%		--verbose
%		--pretty
%
%	setoptions:
%		/* empty */
%		setoption setoptions
%
%	setoption:
%		-P
%		-B
%		-A
%		-f
%		-r
%		-v
%		-p
%		--print
%		--browse
%		--print-all
%		--flat
%		--raw-pretty
%		--verbose
%		--pretty
%
%	varvalue:
%		"depth" num
%		"size" num
%		"clipx" num
%		"clipy" num
%		"format" fmt
%		"num_io_actions" num
%
%	numlist:
%		num
%		num numlist
%
%	fmt:
%		"flat"
%		"raw_pretty"
%		"verbose"
%		"pretty"
%
%	path:
%		["/"] [dirs]
%
%	dirs:
%		dir ["/" dirs]
%
%	dir:
%		num
%		".."
%

:- module mdb.parse.

:- interface.

:- import_module mdb.browser_info.

:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module std_util.
:- import_module string.

:- type command
	--->	print(
			maybe(maybe_option_table(format_option)),
			maybe(path)
		)
	;	cd(path)
	;	cd
	;	track(how_track_subterm, should_assert_invalid, maybe(path))
	;	mode_query(path)
	;	mode_query
	;	pwd
	;	help
	;	set(maybe_option_table(setting_option), setting)
	;	set
	;	quit
	;	display
	;	write
	;	empty
	;	unknown.

:- type path
	--->	root_rel(list(dir))
	;	dot_rel(list(dir)).

:- type format_option
	--->	flat
	;	raw_pretty
	;	verbose
	;	pretty.

:- type setting_option
	--->	print
	;	browse
	;	print_all
	;	flat
	;	raw_pretty
	;	verbose
	;	pretty.

% If the term browser is called from the external debugger, the term browser
% commands are send through the socket via terms of type external_request.
:- type external_request
	---> external_request(string).

:- pred parse__read_command(string::in, command::out, io::di, io::uo) is det.

:- pred parse__read_command_external(command::out, io::di, io::uo) is det.

	% parse(Words, Command).
	% Command is the command give by the list of strings Words.
	%
:- pred parse__parse(list(string)::in, command::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.util.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.

:- type token
	--->	(.)
	;	(..)
	;	(/)
	;	(?)
	;	(^)
	;	(<)
	;	num(int)
	;	name(string)
	;	arg(string)
	;	unknown(char).

parse__read_command(Prompt, Command, !IO) :-
	util__trace_get_command(Prompt, Line, !IO),
	string__words(char__is_whitespace, Line) = Words,
	( parse(Words, Command2) ->
		Command = Command2
	;
		Command = unknown
	).

parse__read_command_external(Command, !IO) :-
	io__read(Result, !IO),
	(
		Result = ok(external_request(StringToParse)),
		string__words(char__is_whitespace, StringToParse) = Words,
		( parse(Words, Command2) ->
			Command = Command2
		;
			Command = unknown
		)
	;
		Result = eof,
		Command = quit
	;
		Result = error(_, _),
		Command = unknown
	).

:- pred lexer_words(list(string)::in, list(token)::out) is det.

lexer_words([], []).
lexer_words([Word | Words], Tokens) :-
	lexer_word(Word, WordTokens),
	lexer_words(Words, WordsTokens),
	list__append(WordTokens, WordsTokens, Tokens).

:- pred lexer_word(string::in, list(token)::out) is det.

lexer_word(Word, Tokens) :-
	string__to_char_list(Word, Chars),
	lexer_word_chars(Chars, Tokens).

:- pred lexer_word_chars(list(char)::in, list(token)::out) is det.

lexer_word_chars([], []).
lexer_word_chars([C | Cs], Toks) :-
	( C = ('.') ->
		lexer_dots(Cs, Toks)
	; C = ('/') ->
		Toks = [(/) | Toks2],
		lexer_word_chars(Cs, Toks2)
	; C = ('?') ->
		Toks = [(?) | Toks2],
		lexer_word_chars(Cs, Toks2)
	; C = ('^') ->
		Toks = [(^) | Toks2],
		lexer_word_chars(Cs, Toks2)
	; C = ('<') ->
		Toks = [(<) | Toks2],
		lexer_word_chars(Cs, Toks2)
	; C = ('-'), Cs = [H | T] ->
		lexer_arg([H | T], Toks)
	; char__is_digit(C) ->
		dig_to_int(C, N),
		lexer_num(N, Cs, Toks)
	; char__is_alpha_or_underscore(C) ->
		lexer_name(C, Cs, Toks)
	; char__is_whitespace(C) ->
		lexer_word_chars(Cs, Toks)
	;
		Toks = [unknown(C) | Toks2],
		lexer_word_chars(Cs, Toks2)
	).

:- pred lexer_dots(list(char)::in, list(token)::out) is det.

lexer_dots([], []).
lexer_dots([C | Cs], Toks) :-
	( C = ('.') ->
		Tok = (..),
		lexer_word_chars(Cs, Toks2),
		Toks = [Tok | Toks2]
	;
		Tok = (.),
		lexer_word_chars([C | Cs], Toks2),
		Toks = [Tok | Toks2]
	).

:- pred dig_to_int(char::in, int::out) is det.

dig_to_int(C, N) :-
	char__to_int('0', Zero),
	char__to_int(C, CN),
	N = CN - Zero.

:- pred lexer_arg(list(char)::in(non_empty_list), list(token)::out) is det.

lexer_arg([Head | Tail], Toks) :-
	( Head = ('-') ->
		string__from_char_list(Tail, ArgName)
	;
		string__from_char_list([Head | Tail], ArgName)
	),
	Toks = [arg(ArgName)].

:- pred lexer_num(int::in, list(char)::in, list(token)::out) is det.

lexer_num(N, Cs, Toks) :-
	list__takewhile(char__is_digit, Cs, Digits, Rest),
	digits_to_int_acc(N, Digits, Num),
	Toks = [num(Num) | Toks2],
	lexer_word_chars(Rest, Toks2).

:- pred digits_to_int_acc(int::in, list(char)::in, int::out) is det.

digits_to_int_acc(Acc, [], Acc).
digits_to_int_acc(Acc, [C | Cs], Num) :-
	dig_to_int(C, D),
	Acc2 = 10 * Acc + D,
	digits_to_int_acc(Acc2, Cs, Num).

:- pred lexer_name(char::in, list(char)::in, list(token)::out) is det.

lexer_name(C, Cs, Toks) :-
	list__takewhile(char__is_alnum_or_underscore, Cs, Letters, Rest),
	string__from_char_list([C | Letters], Name),
	lexer_word_chars(Rest, Toks2),
	Toks = [name(Name) | Toks2].

%---------------------------------------------------------------------------%

parse(Words, Command) :-
	(
		Words = [],
		Command = empty
	;
		Words = [CmdWord | ArgWords],
		lexer_word(CmdWord, CmdTokens),
		lexer_words(ArgWords, ArgTokens),
		( CmdTokens = [_] ->
			% If the initial word is one token, then it can make
			% sense to parse the command line as words.
			MaybeArgWords = yes(ArgWords)
		;
			% If the initial word is more than one token, then
			% it doesn't make sense to parse the command line
			% as words.
			MaybeArgWords = no
		),
		list__append(CmdTokens, ArgTokens, AllTokens),
		(
			AllTokens = [],
			Command = empty
		;
			AllTokens = [FirstToken | LaterTokens],
			parse_cmd(FirstToken, LaterTokens, MaybeArgWords,
				Command)
		)
	).

:- pred parse_cmd(token::in, list(token)::in, maybe(list(string))::in,
	command::out) is semidet.

parse_cmd(CmdToken, ArgTokens, MaybeArgWords, Command) :-
	(
		( CmdToken = name("help")
		; CmdToken = (?)
		; CmdToken = name("h")
		)
	->
		ArgTokens = [],
		Command = help
	;
		( CmdToken = name("cd")
		; CmdToken = (^)
		)
	->
		( ArgTokens = [] ->
			Command = cd
		;
			parse_path(ArgTokens, Path),
			Command = cd(Path)
		)
	;
		CmdToken = name("cdr")
	->
		ArgTokens = [num(Repetitions) | TokenPath],
		list.duplicate(Repetitions, TokenPath, DupTokenPath),
		list.condense(DupTokenPath, RepeatedTokenPath),
		parse_path(RepeatedTokenPath, RepeatedPath),
		Command = cd(RepeatedPath)
	;
		CmdToken = name("pwd")
	->
		ArgTokens = [],
		Command = pwd
	;
		(
			CmdToken = name("track"),
			AssertInvalid = no_assert_invalid
		;
			CmdToken = name("t"),
			AssertInvalid = no_assert_invalid
		;
			CmdToken = name("mark"),
			AssertInvalid = assert_invalid
		;
			CmdToken = name("m"),
			AssertInvalid = assert_invalid
		)
	->
		( ArgTokens = [] ->
			HowTrack = track_fast,
			MaybePath = no
		;
			( ArgTokens = [arg("accurate")] 
			; ArgTokens = [arg("a")] 
			) 
		->
			HowTrack = track_accurate,
			MaybePath = no
		; 
			( ArgTokens = [arg("accurate") | Rest] 
			; ArgTokens = [arg("a") | Rest]
			)
		->
			HowTrack = track_accurate,
			parse_path(Rest, Path),
			MaybePath = yes(Path)
		;
			HowTrack = track_fast,
			parse_path(ArgTokens, Path),
			MaybePath = yes(Path)
		),
		Command = track(HowTrack, AssertInvalid, MaybePath)
	;
		CmdToken = name("mode")
	->
		( ArgTokens = [] ->
			Command = mode_query
		;
			parse_path(ArgTokens, Path),
			Command = mode_query(Path)
		)
	;
		CmdToken = name("set")
	->
		( ArgTokens = [] ->
			Command = set
		;
			MaybeArgWords = yes(ArgWords),
			OptionOps = option_ops_multi(short_setting_option,
				long_setting_option, setting_option_defaults),
			getopt__process_options(OptionOps, ArgWords,
				RemainingWords, MaybeOptionTable),
			lexer_words(RemainingWords, RemainingTokens),
			parse_setting(RemainingTokens, Setting),
			Command = set(MaybeOptionTable, Setting)
		)
	;
		CmdToken = name("quit")
	->
		ArgTokens = [],
		Command = quit
	;
		( CmdToken = name("display")
		; CmdToken = name("d")
		)
	->
		ArgTokens = [],
		Command = display
	;
		( CmdToken = name("write")
		; CmdToken = name("w")
		)
	->
		ArgTokens = [],
		Command = write
	;
		( CmdToken = name("print")
		; CmdToken = name("p")
		; CmdToken = name("ls")
		)
	->
		(
			MaybeArgWords = no,
			MaybeMaybeOptionTable = no,
			RemainingTokens = ArgTokens
		;
			MaybeArgWords = yes(ArgWords),
			OptionOps = option_ops_multi(short_format_option,
				long_format_option, format_option_defaults),
			getopt__process_options(OptionOps, ArgWords,
				RemainingWords, MaybeOptionTable),
			MaybeMaybeOptionTable = yes(MaybeOptionTable),
			lexer_words(RemainingWords, RemainingTokens)
		),
		( RemainingTokens = [] ->
			MaybePath = no
		;
			parse_path(RemainingTokens, Path),
			MaybePath = yes(Path)
		),
		Command = print(MaybeMaybeOptionTable, MaybePath)
	;
		CmdToken = (<)
	->
		ArgTokens = [num(Depth)],
		% compute the default MaybeOptionTable
		OptionOps = option_ops_multi(short_setting_option,
			long_setting_option, setting_option_defaults),
		getopt__process_options(OptionOps, [], _, MaybeOptionTable),
		Command = set(MaybeOptionTable, depth(Depth))
	;
		fail
	).

:- pred parse_path(list(token)::in, path::out) is semidet.

	% SICStus is forgiving in the syntax of paths, hence so are we.
	% XXX: Be less forgiving?
parse_path([Token | Tokens], Path) :-
	( Token = (/) ->
		Path = root_rel(Dirs),
		parse_dirs(Tokens, Dirs)
	;
		Path = dot_rel(Dirs),
		parse_dirs([Token | Tokens], Dirs)
	).

:- pred parse_dirs(list(token)::in, list(dir)::out) is semidet.

parse_dirs([], []).
parse_dirs([Token | Tokens], Dirs) :-
	(
		Token = num(Subdir),
		Dirs = [child_num(Subdir) | RestDirs],
		parse_dirs(Tokens, RestDirs)
	;
		Token = name(NamedSubdir),
		Dirs = [child_name(NamedSubdir) | RestDirs],
		parse_dirs(Tokens, RestDirs)
	;
		Token = (..),
		Dirs = [parent | RestDirs],
		parse_dirs(Tokens, RestDirs)
	;
		% We can effectively ignore slashes (for Unix-style
		% pathnames) and carets (for SICStus-style pathnames),
		% but anything else is not allowed.
		Token = (/),
		parse_dirs(Tokens, Dirs)
	;
		Token = (^),
		parse_dirs(Tokens, Dirs)
	).

:- pred parse_setting(list(token)::in, setting::out) is semidet.

parse_setting([Token | Tokens], Setting) :-
	( Token = name("depth") ->
		Tokens = [num(Depth)],
		Setting = depth(Depth)
	; Token = name("size") ->
		Tokens = [num(Size)],
		Setting = size(Size)
	; Token = name("width") ->
		Tokens = [num(X)],
		Setting = width(X)
	; Token = name("lines") ->
		Tokens = [num(Y)],
		Setting = lines(Y)
	; Token = name("num_io_actions") ->
		Tokens = [num(Y)],
		Setting = num_io_actions(Y)
	; Token = name("format") ->
		Tokens = [Fmt],
		( Fmt = name("flat") ->
			Setting = format(flat)
		; Fmt = name("raw_pretty") ->
			Setting = format(raw_pretty)
		; Fmt = name("verbose") ->
			Setting = format(verbose)
		;
			Fmt = name("pretty"),
			Setting = format(pretty)
		)
	;
		fail
	).

%---------------------------------------------------------------------------%

:- pred short_format_option(char::in, format_option::out) is semidet.

short_format_option('f', flat).
short_format_option('r', raw_pretty).
short_format_option('v', verbose).
short_format_option('p', pretty).

:- pred long_format_option(string::in, format_option::out) is semidet.

long_format_option("flat", flat).
long_format_option("raw-pretty", raw_pretty).
long_format_option("verbose", verbose).
long_format_option("pretty", pretty).

:- pred format_option_defaults(format_option::out, option_data::out) is multi.

format_option_defaults(flat,		bool(no)).
format_option_defaults(raw_pretty,	bool(no)).
format_option_defaults(verbose,		bool(no)).
format_option_defaults(pretty,		bool(no)).

%---------------------------------------------------------------------------%

:- pred short_setting_option(char::in, setting_option::out) is semidet.

short_setting_option('P', print).
short_setting_option('B', browse).
short_setting_option('A', print_all).
short_setting_option('f', flat).
short_setting_option('r', raw_pretty).
short_setting_option('v', verbose).
short_setting_option('p', pretty).

:- pred long_setting_option(string::in, setting_option::out) is semidet.

long_setting_option("print", print).
long_setting_option("browse", browse).
long_setting_option("print-all", print_all).
long_setting_option("flat", flat).
long_setting_option("raw-pretty", raw_pretty).
long_setting_option("verbose", verbose).
long_setting_option("pretty", pretty).

:- pred setting_option_defaults(setting_option::out, option_data::out)
	is multi.

setting_option_defaults(print,		bool(no)).
setting_option_defaults(browse,		bool(no)).
setting_option_defaults(print_all,	bool(no)).
setting_option_defaults(flat,		bool(no)).
setting_option_defaults(raw_pretty,	bool(no)).
setting_option_defaults(verbose,	bool(no)).
setting_option_defaults(pretty,		bool(no)).

%---------------------------------------------------------------------------%

% The commented out code is not currently used.

% :- pred show_command(command::in, io::di, io::uo) is det.
% 
% show_command(ls(Path)) -->
% 	io__write_string("ls "),
% 	show_path(Path),
% 	io__nl.
% show_command(ls) -->
% 	io__write_string("ls\n").
% show_command(cd(Path)) -->
% 	io__write_string("cd "),
% 	show_path(Path),
% 	io__nl.
% show_command(cd) -->
% 	io__write_string("cd\n").
% show_command(track(Path)) -->
% 	io__write_string("track "),
% 	show_path(Path),
% 	io__nl.
% show_command(track) -->
% 	io__write_string("track\n").
% show_command(pwd) -->
% 	io__write_string("pwd\n").
% show_command(help) -->
% 	io__write_string("help\n").
% show_command(set(Setting)) -->
% 	io__write_string("set "),
% 	show_setting(Setting),
% 	io__nl.
% show_command(set) -->
% 	io__write_string("set\n").
% show_command(quit) -->
% 	io__write_string("quit\n").
% show_command(print) -->
% 	io__write_string("print\n").
% show_command(display) -->
% 	io__write_string("display\n").
% show_command(write) -->
% 	io__write_string("write\n").
% show_command(empty) -->
% 	io__write_string("empty\n").
% show_command(unknown) -->
% 	io__write_string("unknown\n").
% 
% :- pred show_path(path::in, io::di, io::uo) is det.
% 
% show_path(root_rel(Dirs)) -->
% 	io__write_string("/"),
% 	show_dirs(Dirs).
% show_path(dot_rel(Dirs)) -->
% 	show_dirs(Dirs).
% 
% :- pred show_dirs(list(dir)::in, io::di, io::uo) is det.
% 
% show_dirs([]) -->
% 	io__nl.
% show_dirs([child_num(Num) | Dirs]) -->
% 	io__write_int(Num),
% 	io__write_string("/"),
% 	show_dirs(Dirs).
% show_dirs([child_name(Name) | Dirs]) -->
% 	io__write_string(Name),
% 	io__write_string("/"),
% 	show_dirs(Dirs).
% show_dirs([parent | Dirs]) -->
% 	io__write_string("../"),
% 	show_dirs(Dirs).
% 
% :- pred show_setting(setting::in, io::di, io::uo) is det.
% 
% show_setting(depth(Depth)) -->
% 	io__write_string("depth "),
% 	io__write_int(Depth),
% 	io__nl.
% show_setting(size(Size)) -->
% 	io__write_string("size "),
% 	io__write_int(Size),
% 	io__nl.
% show_setting(width(X)) -->
% 	io__write_string("width "),
% 	io__write_int(X),
% 	io__nl.
% show_setting(lines(Y)) -->
% 	io__write_string("lines "),
% 	io__write_int(Y),
% 	io__nl.
% show_setting(format(Fmt)) -->
% 	io__write_string("format "),
% 	show_format(Fmt),
% 	io__nl.
% show_setting(num_io_actions(N)) -->
% 	io__write_string("num_io_actions "),
% 	io__write_int(N),
% 	io__nl.
% 
% :- pred show_format(portray_format::in, io::di, io::uo) is det.
% 
% show_format(flat) -->
% 	io__write_string("flat").
% show_format(raw_pretty) -->
% 	io__write_string("raw_pretty").
% show_format(verbose) -->
% 	io__write_string("verbose").
% show_format(pretty) -->
% 	io__write_string("pretty").

%---------------------------------------------------------------------------%
