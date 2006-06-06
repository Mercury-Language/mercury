%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: parse.m.
% Author: aet.
% 
% This file contains the parser for the term browser command language.
% If the term browser is called from mdb, it parses the stuff you type
% at the "browser> " prompt after typing "browse" from the mdb prompt.
% If it is called from the external debugger, then it parses the stuff
% contained in a term `external_request(<string to parse>)' send by the
% external debugger.
% 
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% The Command Language
%
%   commandline:
%       "?"                 // SICStus help
%       "^" [path]          // SICStus cd
%       "d"                 // SICStus display
%       "w"                 // SICStus write
%       "<"                 // SICStus set depth
%       "help"
%       "h"                 // short for help
%       "cd" [path]
%       "pwd"
%       "ls" [formatoptions] [path]
%       "print" [formatoptions] [path]
%       "p" [formatoptions] [path]  // short for print
%       "display"
%       "write"
%       "format" [formatcmdoptions] fmt
%       "depth" [formatparamcmdoptions] value
%       "size" [formatparamcmdoptions] value
%       "width" [formatparamcmdoptions] value
%       "lines" [formatparamcmdoptions] value
%       "num_io_actions" int
%       "params"
%       "track" [--accurate] [path]
%       "t" [--accurate] [path]
%       "mark" [--accurate] [path]
%       "m" [--accurate] [path]
%       "mode" [path]
%       "quit"
%
%   formatoptions:
%       /* empty */
%       formatoption formatoptions
%
%   formatoption:
%       -f
%       -r
%       -v
%       -p
%       --flat
%       --raw-pretty
%       --verbose
%       --pretty
%
%   formatcmdoptions:
%       /* empty */
%       formatcmdoption formatcmdoptions
%
%   formatcmdoption:
%       -P
%       -B
%       -A
%       --print
%       --browse
%       --print-all
%
%   formatparamcmdoptions:
%       /* empty */
%       formatparamcmdoption formatparamcmdoptions
%
%   formatparamcmdoption:
%       -P
%       -B
%       -A
%       -f
%       -r
%       -v
%       -p
%       --print
%       --browse
%       --print-all
%       --flat
%       --raw-pretty
%       --verbose
%       --pretty
%
%   fmt:
%       "flat"
%       "raw_pretty"
%       "verbose"
%       "pretty"
%
%   path:
%       ["/"] [dirs]
%
%   dirs:
%       dir ["/" dirs]
%
%   dir:
%       num
%       ".."
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.parse.
:- interface.

:- import_module mdb.browser_info.

:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

:- type command
    --->    print(maybe(maybe_option_table(format_option)), maybe(path))
    ;       cd(path)
    ;       cd
    ;       track(how_track_subterm, should_assert_invalid, maybe(path))
    ;       mode_query(path)
    ;       mode_query
    ;       pwd
    ;       help
    ;       param_command(param_cmd)
    ;       quit
    ;       display
    ;       write
    ;       empty
    ;       unknown.

:- type format_param_cmd
    --->    param_depth
    ;       param_size
    ;       param_width
    ;       param_lines.

:- type path
    --->    root_rel(list(dir))
    ;       dot_rel(list(dir)).

:- type format_option
    --->    flat
    ;       raw_pretty
    ;       verbose
    ;       pretty.

:- type setting_option
    --->    set_print
    ;       set_browse
    ;       set_print_all
    ;       set_flat
    ;       set_raw_pretty
    ;       set_verbose
    ;       set_pretty.

    % If the term browser is called from the external debugger, the term
    % browser commands are send through the socket via terms of type
    % external_request.
    %
:- type external_request
    ---> external_request(string).

:- pred read_command(string::in, command::out, io::di, io::uo) is det.

:- pred read_command_external(command::out, io::di, io::uo) is det.

    % parse(Words, Command):
    %
    % Command is the command give by the list of strings Words.
    %
:- pred parse(list(string)::in, command::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.

:- type token
    --->    (.)
    ;       (..)
    ;       (/)
    ;       (?)
    ;       (^)
    ;       (<)
    ;       num(int)
    ;       name(string)
    ;       arg(string)
    ;       unknown(char).

read_command(Prompt, Command, !IO) :-
    util.trace_get_command(Prompt, Line, !IO),
    string.words(char.is_whitespace, Line) = Words,
    ( parse(Words, Command2) ->
        Command = Command2
    ;
        Command = unknown
    ).

read_command_external(Command, !IO) :-
    io.read(Result, !IO),
    (
        Result = ok(external_request(StringToParse)),
        string.words(char.is_whitespace, StringToParse) = Words,
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
    list.append(WordTokens, WordsTokens, Tokens).

:- pred lexer_word(string::in, list(token)::out) is det.

lexer_word(Word, Tokens) :-
    string.to_char_list(Word, Chars),
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
    ; char.is_digit(C) ->
        dig_to_int(C, N),
        lexer_num(N, Cs, Toks)
    ; char.is_alpha_or_underscore(C) ->
        lexer_name(C, Cs, Toks)
    ; char.is_whitespace(C) ->
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
    char.to_int('0', Zero),
    char.to_int(C, CN),
    N = CN - Zero.

:- pred lexer_arg(list(char)::in(non_empty_list), list(token)::out) is det.

lexer_arg([Head | Tail], Toks) :-
    ( Head = ('-') ->
        string.from_char_list(Tail, ArgName)
    ;
        string.from_char_list([Head | Tail], ArgName)
    ),
    Toks = [arg(ArgName)].

:- pred lexer_num(int::in, list(char)::in, list(token)::out) is det.

lexer_num(N, Cs, Toks) :-
    list.takewhile(char.is_digit, Cs, Digits, Rest),
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
    list.takewhile(char.is_alnum_or_underscore, Cs, Letters, Rest),
    string.from_char_list([C | Letters], Name),
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
        list.append(CmdTokens, ArgTokens, AllTokens),
        (
            AllTokens = [],
            Command = empty
        ;
            AllTokens = [FirstToken | LaterTokens],
            parse_cmd(FirstToken, LaterTokens, MaybeArgWords, Command)
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
        (
            ArgTokens = []
        ->
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
        (
            ArgTokens = [],
            Command = mode_query
        ;
            ArgTokens = [_ | _],
            parse_path(ArgTokens, Path),
            Command = mode_query(Path)
        )
    ;
        CmdToken = name("format")
    ->
        (
            ArgTokens = [],
            Command = param_command(print_params)
        ;
            ArgTokens = [_ | _],
            MaybeArgWords = yes(ArgWords),
            OptionOps = option_ops_multi(short_format_cmd_option,
                long_format_cmd_option, format_cmd_option_defaults),
            getopt.process_options(OptionOps, ArgWords,
                RemainingWords, MaybeOptionTable),
            lexer_words(RemainingWords, RemainingTokens),
            parse_format(RemainingTokens, Setting),
            Command = param_command(format(MaybeOptionTable, Setting))
        )
    ;
        (
            CmdToken = name("depth"),
            ParamCmd = param_depth
        ;
            CmdToken = name("size"),
            ParamCmd = param_size
        ;
            CmdToken = name("width"),
            ParamCmd = param_width
        ;
            CmdToken = name("lines"),
            ParamCmd = param_lines
        )
    ->
        (
            ArgTokens = [],
            Command = param_command(print_params)
        ;
            ArgTokens = [_ | _],
            MaybeArgWords = yes(ArgWords),
            OptionOps = option_ops_multi(short_format_param_cmd_option,
                long_format_param_cmd_option,
                format_param_cmd_option_defaults),
            getopt.process_options(OptionOps, ArgWords,
                RemainingWords, MaybeOptionTable),
            lexer_words(RemainingWords, RemainingTokens),
            RemainingTokens = [num(N)],
            param_cmd_to_setting(ParamCmd, N, Setting),
            Command = param_command(format_param(MaybeOptionTable, Setting))
        )
    ;
        CmdToken = name("params")
    ->
        Command = param_command(print_params)
    ;
        CmdToken = name("num_io_actions")
    ->
        ArgTokens = [num(N)],
        Command = param_command(num_io_actions(N))
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
            getopt.process_options(OptionOps, ArgWords,
                RemainingWords, MaybeOptionTable),
            MaybeMaybeOptionTable = yes(MaybeOptionTable),
            lexer_words(RemainingWords, RemainingTokens)
        ),
        (
            RemainingTokens = [],
            MaybePath = no
        ;
            RemainingTokens = [_ | _],
            parse_path(RemainingTokens, Path),
            MaybePath = yes(Path)
        ),
        Command = print(MaybeMaybeOptionTable, MaybePath)
    ;
        CmdToken = (<)
    ->
        ArgTokens = [num(Depth)],
        OptionOps = option_ops_multi(short_format_param_cmd_option,
            long_format_param_cmd_option, format_param_cmd_option_defaults),
        getopt.process_options(OptionOps, [], _, MaybeOptionTable),
        Command = param_command(format_param(MaybeOptionTable, depth(Depth)))
    ;
        fail
    ).

:- pred param_cmd_to_setting(format_param_cmd::in, int::in, setting::out)
    is det.

param_cmd_to_setting(param_depth, N, depth(N)).
param_cmd_to_setting(param_size,  N, size(N)).
param_cmd_to_setting(param_width, N, width(N)).
param_cmd_to_setting(param_lines, N, lines(N)).

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

:- pred parse_format(list(token)::in, setting::out) is semidet.

parse_format([Fmt], Setting) :-
    ( Fmt = name("flat") ->
        Setting = format(flat)
    ; Fmt = name("raw_pretty") ->
        Setting = format(raw_pretty)
    ; Fmt = name("verbose") ->
        Setting = format(verbose)
    ; Fmt = name("pretty") ->
        Setting = format(pretty)
    ;
        fail
    ).

:- pred parse_format_param(list(token)::in, setting::out) is semidet.

parse_format_param([Token | Tokens], Setting) :-
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

format_option_defaults(flat,        bool(no)).
format_option_defaults(raw_pretty,  bool(no)).
format_option_defaults(verbose,     bool(no)).
format_option_defaults(pretty,      bool(no)).

%---------------------------------------------------------------------------%

:- pred short_format_cmd_option(char::in, setting_option::out) is semidet.

short_format_cmd_option('P', set_print).
short_format_cmd_option('B', set_browse).
short_format_cmd_option('A', set_print_all).

:- pred long_format_cmd_option(string::in, setting_option::out) is semidet.

long_format_cmd_option("print", set_print).
long_format_cmd_option("browse", set_browse).
long_format_cmd_option("print-all", set_print_all).

:- pred format_cmd_option_defaults(setting_option::out, option_data::out)
    is multi.

format_cmd_option_defaults(set_print,      bool(no)).
format_cmd_option_defaults(set_browse,     bool(no)).
format_cmd_option_defaults(set_print_all,  bool(no)).
format_cmd_option_defaults(set_flat,       bool(no)).
format_cmd_option_defaults(set_raw_pretty, bool(no)).
format_cmd_option_defaults(set_verbose,    bool(no)).
format_cmd_option_defaults(set_pretty,     bool(no)).

%---------------------------------------------------------------------------%

:- pred short_format_param_cmd_option(char::in, setting_option::out)
    is semidet.

short_format_param_cmd_option('P', set_print).
short_format_param_cmd_option('B', set_browse).
short_format_param_cmd_option('A', set_print_all).
short_format_param_cmd_option('f', set_flat).
short_format_param_cmd_option('r', set_raw_pretty).
short_format_param_cmd_option('v', set_verbose).
short_format_param_cmd_option('p', set_pretty).

:- pred long_format_param_cmd_option(string::in, setting_option::out)
    is semidet.

long_format_param_cmd_option("print", set_print).
long_format_param_cmd_option("browse", set_browse).
long_format_param_cmd_option("print-all", set_print_all).
long_format_param_cmd_option("flat", set_flat).
long_format_param_cmd_option("raw-pretty", set_raw_pretty).
long_format_param_cmd_option("verbose", set_verbose).
long_format_param_cmd_option("pretty", set_pretty).

:- pred format_param_cmd_option_defaults(setting_option::out,
    option_data::out) is multi.

format_param_cmd_option_defaults(set_print,      bool(no)).
format_param_cmd_option_defaults(set_browse,     bool(no)).
format_param_cmd_option_defaults(set_print_all,  bool(no)).
format_param_cmd_option_defaults(set_flat,       bool(no)).
format_param_cmd_option_defaults(set_raw_pretty, bool(no)).
format_param_cmd_option_defaults(set_verbose,    bool(no)).
format_param_cmd_option_defaults(set_pretty,     bool(no)).

%---------------------------------------------------------------------------%

% The commented out code is not currently used.

% :- pred show_command(command::in, io::di, io::uo) is det.
% 
% show_command(ls(Path)) -->
%   io.write_string("ls "),
%   show_path(Path),
%   io.nl.
% show_command(ls) -->
%   io.write_string("ls\n").
% show_command(cd(Path)) -->
%   io.write_string("cd "),
%   show_path(Path),
%   io.nl.
% show_command(cd) -->
%   io.write_string("cd\n").
% show_command(track(Path)) -->
%   io.write_string("track "),
%   show_path(Path),
%   io.nl.
% show_command(track) -->
%   io.write_string("track\n").
% show_command(pwd) -->
%   io.write_string("pwd\n").
% show_command(help) -->
%   io.write_string("help\n").
% show_command(quit) -->
%   io.write_string("quit\n").
% show_command(print) -->
%   io.write_string("print\n").
% show_command(display) -->
%   io.write_string("display\n").
% show_command(write) -->
%   io.write_string("write\n").
% show_command(empty) -->
%   io.write_string("empty\n").
% show_command(unknown) -->
%   io.write_string("unknown\n").
% 
% :- pred show_path(path::in, io::di, io::uo) is det.
% 
% show_path(root_rel(Dirs)) -->
%   io.write_string("/"),
%   show_dirs(Dirs).
% show_path(dot_rel(Dirs)) -->
%   show_dirs(Dirs).
% 
% :- pred show_dirs(list(dir)::in, io::di, io::uo) is det.
% 
% show_dirs([]) -->
%   io.nl.
% show_dirs([child_num(Num) | Dirs]) -->
%   io.write_int(Num),
%   io.write_string("/"),
%   show_dirs(Dirs).
% show_dirs([child_name(Name) | Dirs]) -->
%   io.write_string(Name),
%   io.write_string("/"),
%   show_dirs(Dirs).
% show_dirs([parent | Dirs]) -->
%   io.write_string("../"),
%   show_dirs(Dirs).
% 
% :- pred show_setting(setting::in, io::di, io::uo) is det.
% 
% show_setting(depth(Depth)) -->
%   io.write_string("depth "),
%   io.write_int(Depth),
%   io.nl.
% show_setting(size(Size)) -->
%   io.write_string("size "),
%   io.write_int(Size),
%   io.nl.
% show_setting(width(X)) -->
%   io.write_string("width "),
%   io.write_int(X),
%   io.nl.
% show_setting(lines(Y)) -->
%   io.write_string("lines "),
%   io.write_int(Y),
%   io.nl.
% show_setting(format(Fmt)) -->
%   io.write_string("format "),
%   show_format(Fmt),
%   io.nl.
% show_setting(num_io_actions(N)) -->
%   io.write_string("num_io_actions "),
%   io.write_int(N),
%   io.nl.
% 
% :- pred show_format(portray_format::in, io::di, io::uo) is det.
% 
% show_format(flat) -->
%   io.write_string("flat").
% show_format(raw_pretty) -->
%   io.write_string("raw_pretty").
% show_format(verbose) -->
%   io.write_string("verbose").
% show_format(pretty) -->
%   io.write_string("pretty").

%---------------------------------------------------------------------------%
