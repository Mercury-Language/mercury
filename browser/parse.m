%---------------------------------------------------------------------------%
% Copyright (C) 1998-2002 The University of Melbourne.
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
%		"?"			// SICStus help
%		"^" [numlist]		// SICStus cd
%		"d"			// SICStus display
%		"w"			// SICStus write
%		"help"
%		"cd" [path]
%		"pwd"
%		"ls"
%		"display"
%		"write"
%		"set" [varvalue]
%		"mark" [path]
%		"quit"
%
%	varvalue:
%		"depth" num
%		"size" num
%		"clipx" num
%		"clipy" num
%		"format" fmt
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

:- module mdb__parse.

:- interface.

:- import_module io, string, list.
:- import_module mdb__browser_info.

:- type command
	--->	ls(path)
	;	ls
	;	cd(path)
	;	cd
	;	mark(path)
	;	mark
	;	pwd
	;	help
	;	set(setting)
	;	set
	;	quit
	;	print
	;	display
	;	write
	;	empty
	;	unknown.

:- type path
	--->	root_rel(list(dir))
	;	dot_rel(list(dir)).

% If the term browser is called from the external debugger, the term browser
% commands are send through the socket via terms of type external_request.
:- type external_request 
	---> external_request(string).

:- pred parse__read_command(string, command, io__state, io__state).
:- mode parse__read_command(in, out, di, uo) is det.

:- pred parse__read_command_external(command, io__state, io__state).
:- mode parse__read_command_external(out, di, uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module list, char, int, std_util.
:- import_module mdb__util.


:- type token
	--->	(.)
	;	(..)
	;	(/)
	;	(?)
	;	(^)
	;	(<)
	;	num(int)
	;	name(string)
	;	unknown(char)
	.

parse__read_command(Prompt, Command) -->
	util__trace_get_command(Prompt, Line),
	{ string__to_char_list(Line, Cs) },
	{ lexer(Cs, Tokens) },
	( { parse(Tokens, Command2) } ->
		{ Command = Command2 }
	;
		{ Command = unknown }
	).

parse__read_command_external(Command) -->
	io__read(Result),
	( { Result = ok(external_request(StringToParse)) } ->
		{ string__to_char_list(StringToParse, Cs) },
		{ lexer(Cs, Tokens) },
		( { parse(Tokens, Command2) } ->
			{ Command = Command2 }
		;
			{ Command = unknown }
		)
	; { Result = eof } ->
		{ Command = quit }
	;
		{ Command = unknown }
	).

:- pred lexer(list(char), list(token)).
:- mode lexer(in, out) is det.
lexer([], []).
lexer([C | Cs], Toks) :-
	( C = ('.') ->
		lexer_dots(Cs, Toks)
	; C = ('/') ->
		Toks = [(/) | Toks2],
		lexer(Cs, Toks2)
	; C = ('?') ->
		Toks = [(?) | Toks2],
		lexer(Cs, Toks2)
	; C = ('^') ->
		Toks = [(^) | Toks2],
		lexer(Cs, Toks2)
	; C = ('<') ->
		Toks = [(<) | Toks2],
		lexer(Cs, Toks2)
	; char__is_digit(C) ->
		dig_to_int(C, N),
		lexer_num(N, Cs, Toks)
	; char__is_alpha_or_underscore(C) ->
		lexer_name(C, Cs, Toks)
	; char__is_whitespace(C) ->
		lexer(Cs, Toks)
	;
		Toks = [unknown(C) | Toks2],
		lexer(Cs, Toks2)
	).

:- pred lexer_dots(list(char), list(token)).
:- mode lexer_dots(in, out) is det.
lexer_dots([], []).
lexer_dots([C | Cs], Toks) :-
	( C = ('.') ->
		Tok = (..),
		lexer(Cs, Toks2),
		Toks = [Tok | Toks2]
	;
		Tok = (.),
		lexer([C | Cs], Toks2),
		Toks = [Tok | Toks2]
	).

:- pred dig_to_int(char, int).
:- mode dig_to_int(in, out) is det.
dig_to_int(C, N) :-
	char__to_int('0', Zero),
	char__to_int(C, CN),
	N is CN - Zero.

:- pred lexer_num(int, list(char), list(token)).
:- mode lexer_num(in, in, out) is det.
lexer_num(N, Cs, Toks) :-
	list__takewhile(char__is_digit, Cs, Digits, Rest),
	digits_to_int_acc(N, Digits, Num),
	Toks = [num(Num) | Toks2],
	lexer(Rest, Toks2).
	
			
:- pred digits_to_int_acc(int, list(char), int).
:- mode digits_to_int_acc(in, in, out) is det.
digits_to_int_acc(Acc, [], Acc).
digits_to_int_acc(Acc, [C | Cs], Num) :-
	dig_to_int(C, D),
	Acc2 is 10 * Acc + D,
	digits_to_int_acc(Acc2, Cs, Num).
	

:- pred lexer_name(char, list(char), list(token)).
:- mode lexer_name(in, in, out) is det.
lexer_name(C, Cs, Toks) :-
	list__takewhile(char__is_alnum_or_underscore, Cs, Letters, Rest),
	string__from_char_list([C | Letters], Name),
	lexer(Rest, Toks2),
	Toks = [name(Name) | Toks2].
	

:- pred parse(list(token), command).
:- mode parse(in, out) is semidet.
parse(Toks, Command) :-
	( Toks = [] ->
		Command = empty
	;
		start(Toks, Command)
	).

:- pred start(list(token), command).
:- mode start(in, out) is semidet.
start([Tok | Toks], Command) :-
	( (Tok = name("help") ; Tok = (?) ; Tok = name("h")) ->
		Toks = [],
		Command = help
	; (Tok = name("cd") ; Tok = (^)) ->
		( Toks = [] ->
			Command = cd
		;
			parse_path(Toks, Path),
			Command = cd(Path)
		)
	; Tok = name("pwd") ->
		Toks = [],
		Command = pwd
	; Tok = name("ls") ->
		( Toks = [] ->
			Command = ls
		;
			parse_path(Toks, Path),
			Command = ls(Path)
		)
	; Tok = name("mark") ->
		( Toks = [] ->
			Command = mark
		;
			parse_path(Toks, Path),
			Command = mark(Path)
		)
	; Tok = name("set") ->
		( Toks = [] ->
			Command = set
		;
			parse_setting(Toks, Setting),
			Command = set(Setting)
		)
	; Tok = name("quit") ->
		Toks = [],
		Command = quit
	; (Tok = name("display") ; Tok = name("d")) ->
		Toks = [],
		Command = display
	; (Tok = name("write") ; Tok = name("w")) ->
		Toks = [],
		Command = write
	; (Tok = name("print") ; Tok = name("p")) ->
		Toks = [],
		Command = print
	;
		Tok = (<),
		Toks = [num(Depth)],
		Command = set(depth(Depth))
	).

:- pred parse_path(list(token), path).
:- mode parse_path(in, out) is semidet.
	% SICStus is forgiving in the syntax of paths, hence so are we.
	% XXX: Be less forgiving?
parse_path([Tok | Toks], Path) :-
	( Tok = (/) ->
		Path = root_rel(Dirs),
		parse_dirs(Toks, Dirs)
	;
		Path = dot_rel(Dirs),
		parse_dirs([Tok | Toks], Dirs)
	).

:- pred parse_dirs(list(token), list(dir)).
:- mode parse_dirs(in, out) is semidet.
parse_dirs([], []).
parse_dirs([Tok | Toks], Dirs) :-
	(
		Tok = num(Subdir),
		Dirs = [child_num(Subdir) | RestDirs],
		parse_dirs(Toks, RestDirs)
	;
		Tok = name(NamedSubdir),
		Dirs = [child_name(NamedSubdir) | RestDirs],
		parse_dirs(Toks, RestDirs)
	;
		Tok = (..),
		Dirs = [parent | RestDirs],
		parse_dirs(Toks, RestDirs)
	;
		% We can effectively ignore slashes (for Unix-style
		% pathnames) and carets (for SICStus-style pathnames),
		% but anything else is not allowed.
		Tok = (/),
		parse_dirs(Toks, Dirs)
	;
		Tok = (^),
		parse_dirs(Toks, Dirs)
	).

:- pred parse_setting(list(token), setting).
:- mode parse_setting(in, out) is semidet.
parse_setting([Tok | Toks], Setting) :-
	( Tok = name("depth") ->
		Toks = [num(Depth)],
		Setting = depth(Depth)
	; Tok = name("size") ->
		Toks = [num(Size)],
		Setting = size(Size)
	; Tok = name("width") ->
		Toks = [num(X)],
		Setting = width(X)
	; Tok = name("lines") ->
		Toks = [num(Y)],
		Setting = lines(Y)
	; Tok = name("format") ->
		Toks = [Fmt],
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

:- pred show_command(command, io__state, io__state).
:- mode show_command(in, di, uo) is det.
show_command(ls(Path)) -->
	io__write_string("ls "),
	show_path(Path),
	io__nl.
show_command(ls) -->
	io__write_string("ls\n").
show_command(cd(Path)) -->
	io__write_string("cd "),
	show_path(Path),
	io__nl.
show_command(cd) -->
	io__write_string("cd\n").
show_command(mark(Path)) -->
	io__write_string("mark "),
	show_path(Path),
	io__nl.
show_command(mark) -->
	io__write_string("mark\n").
show_command(pwd) -->
	io__write_string("pwd\n").
show_command(help) -->
	io__write_string("help\n").
show_command(set(Setting)) -->
	io__write_string("set "),
	show_setting(Setting),
	io__nl.
show_command(set) -->
	io__write_string("set\n").
show_command(quit) -->
	io__write_string("quit\n").
show_command(print) -->
	io__write_string("print\n").
show_command(display) -->
	io__write_string("display\n").
show_command(write) -->
	io__write_string("write\n").
show_command(empty) -->
	io__write_string("empty\n").
show_command(unknown) -->
	io__write_string("unknown\n").

:- pred show_path(path, io__state, io__state).
:- mode show_path(in, di, uo) is det.
show_path(root_rel(Dirs)) -->
	io__write_string("/"),
	show_dirs(Dirs).
show_path(dot_rel(Dirs)) -->
	show_dirs(Dirs).

:- pred show_dirs(list(dir), io__state, io__state).
:- mode show_dirs(in, di, uo) is det.
show_dirs([]) -->
	io__nl.
show_dirs([child_num(Num) | Dirs]) -->
	io__write_int(Num),
	io__write_string("/"),
	show_dirs(Dirs).
show_dirs([child_name(Name) | Dirs]) -->
	io__write_string(Name),
	io__write_string("/"),
	show_dirs(Dirs).
show_dirs([parent | Dirs]) -->
	io__write_string("../"),
	show_dirs(Dirs).

:- pred show_setting(setting, io__state, io__state).
:- mode show_setting(in, di, uo) is det.
show_setting(depth(Depth)) -->
	io__write_string("depth "),
	io__write_int(Depth),
	io__nl.
show_setting(size(Size)) -->
	io__write_string("size "),
	io__write_int(Size),
	io__nl.
show_setting(width(X)) -->
	io__write_string("width "),
	io__write_int(X),
	io__nl.
show_setting(lines(Y)) -->
	io__write_string("lines "),
	io__write_int(Y),
	io__nl.
show_setting(format(Fmt)) -->
	io__write_string("format "),
	show_format(Fmt),
	io__nl.

:- pred show_format(portray_format, io__state, io__state).
:- mode show_format(in, di, uo) is det.
show_format(flat) -->
	io__write_string("flat").
show_format(raw_pretty) -->
	io__write_string("raw_pretty").
show_format(verbose) -->
	io__write_string("verbose").
show_format(pretty) -->
	io__write_string("pretty").

%---------------------------------------------------------------------------%
