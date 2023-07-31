%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2007, 2009 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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

%---------------------------------------------------------------------------%

:- type command
    --->    cmd_print(maybe(maybe_option_table(format_option)), maybe(path))
    ;       cmd_display
    ;       cmd_write
    ;       cmd_memory_addr(maybe(path))
    ;       cmd_cd_path(path)
    ;       cmd_cd_no_path
    ;       cmd_pwd
    ;       cmd_track(how_track_subterm, should_assert_invalid, maybe(path))
    ;       cmd_mode_query(path)
    ;       cmd_mode_query_no_path
    ;       cmd_param(param_cmd)
    ;       cmd_help
    ;       cmd_quit
    ;       cmd_empty
    ;       cmd_unknown.

:- type format_param_cmd
    --->    param_depth
    ;       param_size
    ;       param_width
    ;       param_lines.

:- type path
    --->    root_rel(list(up_down_dir))
    ;       dot_rel(list(up_down_dir)).

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
    --->    external_request(string).

:- pred read_browser_command(io.text_input_stream::in,
    io.text_output_stream::in, string::in, command::out,
    io::di, io::uo) is det.

:- pred read_browser_command_external(io.text_input_stream::in, command::out,
    io::di, io::uo) is det.

    % parse_browser_command_words(Words, Command):
    %
    % Command is the command give by the list of strings Words.
    %
:- pred parse_browser_command_words(list(string)::in, command::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.util.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module string.

:- type token
    --->    token_dot
    ;       token_dot_dot
    ;       token_slash
    ;       token_question
    ;       token_up
    ;       token_lessthan
    ;       token_num(int)
    ;       token_name(string)
    ;       token_arg(string)
    ;       token_unknown(char).

read_browser_command(MdbIn, MdbOut, Prompt, Command, !IO) :-
    util.trace_get_command(MdbIn, MdbOut, Prompt, Line, !IO),
    string.words_separator(char.is_whitespace, Line) = Words,
    ( if parse_browser_command_words(Words, CommandPrime) then
        Command = CommandPrime
    else
        Command = cmd_unknown
    ).

read_browser_command_external(InputStream, Command, !IO) :-
    io.read(InputStream, Result, !IO),
    (
        Result = ok(external_request(StringToParse)),
        string.words_separator(char.is_whitespace, StringToParse) = Words,
        ( if parse_browser_command_words(Words, CommandPrime) then
            Command = CommandPrime
        else
            Command = cmd_unknown
        )
    ;
        Result = eof,
        Command = cmd_quit
    ;
        Result = error(_, _),
        Command = cmd_unknown
    ).

%---------------------------------------------------------------------------%

:- pred lexer_words(list(string)::in, list(token)::out) is det.

lexer_words([], []).
lexer_words([Word | Words], Tokens) :-
    lexer_word(Word, WordTokens),
    lexer_words(Words, WordsTokens),
    Tokens = WordTokens ++ WordsTokens.

:- pred lexer_word(string::in, list(token)::out) is det.

lexer_word(Word, Tokens) :-
    string.to_char_list(Word, Chars),
    lexer_word_chars(Chars, Tokens).

:- pred lexer_word_chars(list(char)::in, list(token)::out) is det.

lexer_word_chars([], []).
lexer_word_chars([C | Cs], Tokens) :-
    ( if C = ('.') then
        lexer_dots(Cs, Tokens)
    else if
        ( C = ('/'), Token = token_slash
        ; C = ('?'), Token = token_question
        ; C = ('^'), Token = token_up
        ; C = ('<'), Token = token_lessthan
        )
    then
        lexer_word_chars(Cs, TailTokens),
        Tokens = [Token | TailTokens]
    else if C = ('-'), Cs = [H | T] then
        lexer_arg([H | T], Tokens)
    else if char.is_digit(C) then
        dig_to_int(C, N),
        lexer_num(N, Cs, Tokens)
    else if char.is_alpha_or_underscore(C) then
        lexer_name(C, Cs, Tokens)
    else if char.is_whitespace(C) then
        lexer_word_chars(Cs, Tokens)
    else
        lexer_word_chars(Cs, TailTokens),
        Tokens = [token_unknown(C) | TailTokens]
    ).

:- pred lexer_dots(list(char)::in, list(token)::out) is det.

lexer_dots([], []).
lexer_dots([C | Cs], Tokens) :-
    ( if C = ('.') then
        lexer_word_chars(Cs, TailTokens),
        Tokens = [token_dot_dot | TailTokens]
    else
        lexer_word_chars([C | Cs], TailTokens),
        Tokens = [token_dot | TailTokens]
    ).

:- pred dig_to_int(char::in, int::out) is det.

dig_to_int(C, N) :-
    char.to_int('0', Zero),
    char.to_int(C, CN),
    N = CN - Zero.

:- pred lexer_arg(list(char)::in(non_empty_list), list(token)::out) is det.

lexer_arg([Head | Tail], Tokens) :-
    ( if Head = ('-') then
        string.from_char_list(Tail, ArgName)
    else
        string.from_char_list([Head | Tail], ArgName)
    ),
    Tokens = [token_arg(ArgName)].

:- pred lexer_num(int::in, list(char)::in, list(token)::out) is det.

lexer_num(N, Cs, Tokens) :-
    list.take_while(char.is_digit, Cs, Digits, TailCs),
    digits_to_int_acc(N, Digits, Num),
    lexer_word_chars(TailCs, TailTokens),
    Tokens = [token_num(Num) | TailTokens].

:- pred digits_to_int_acc(int::in, list(char)::in, int::out) is det.

digits_to_int_acc(Acc, [], Acc).
digits_to_int_acc(Acc, [C | Cs], Num) :-
    dig_to_int(C, Digit),
    NextAcc = 10 * Acc + Digit,
    digits_to_int_acc(NextAcc, Cs, Num).

:- pred lexer_name(char::in, list(char)::in, list(token)::out) is det.

lexer_name(C, Cs, Tokens) :-
    list.take_while(char.is_alnum_or_underscore, Cs, LetterCs, TailCs),
    string.from_char_list([C | LetterCs], Name),
    lexer_word_chars(TailCs, TailTokens),
    Tokens = [token_name(Name) | TailTokens].

%---------------------------------------------------------------------------%

parse_browser_command_words(Words, Command) :-
    (
        Words = [],
        Command = cmd_empty
    ;
        Words = [CmdWord | ArgWords],
        lexer_word(CmdWord, CmdTokens),
        lexer_words(ArgWords, ArgTokens),
        ( if CmdTokens = [_] then
            % If the initial word is one token, then it can make sense
            % to parse the command line as words.
            MaybeArgWords = yes(ArgWords)
        else
            % If the initial word is more than one token, then it doesn't
            % make sense to parse the command line as words.
            MaybeArgWords = no
        ),
        AllTokens = CmdTokens ++ ArgTokens,
        (
            AllTokens = [],
            Command = cmd_empty
        ;
            AllTokens = [FirstToken | LaterTokens],
            parse_browser_command_tokens(FirstToken, LaterTokens,
                MaybeArgWords, Command)
        )
    ).

:- pred parse_browser_command_tokens(token::in, list(token)::in,
    maybe(list(string))::in, command::out) is semidet.

parse_browser_command_tokens(CmdToken, ArgTokens, MaybeArgWords, Command) :-
    % Please keep the code recognizing commands in the same order
    % as the definition of the command type.

    % If you add more commands, please update the documentation printed
    % by the help predicate in browse.m.
    (
        ( CmdToken = token_name("print")
        ; CmdToken = token_name("p")
        ; CmdToken = token_name("ls")
        ),
        (
            MaybeArgWords = no,
            MaybeMaybeOptionTable = no,
            RemainingTokens = ArgTokens
        ;
            MaybeArgWords = yes(ArgWords),
            OptionOps = option_ops_multi(short_format_option,
                long_format_option, format_option_defaults),
            getopt.process_options(OptionOps, ArgWords,
                RemainingWords, MaybeOptionTable0),
            MaybeOptionTable =
                convert_to_maybe_option_table(MaybeOptionTable0),
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
        Command = cmd_print(MaybeMaybeOptionTable, MaybePath)
    ;
        ( CmdToken = token_name("display")
        ; CmdToken = token_name("d")
        ),
        ArgTokens = [],
        Command = cmd_display
    ;
        ( CmdToken = token_name("write")
        ; CmdToken = token_name("w")
        ),
        ArgTokens = [],
        Command = cmd_write
    ;
        ( CmdToken = token_name("memory_addr")
        ; CmdToken = token_name("addr")
        ),
        % The abbreviations "m" and "a" are both taken.
        (
            ArgTokens = [],
            MaybePath = no
        ;
            ArgTokens = [_ | _],
            parse_path(ArgTokens, Path),
            MaybePath = yes(Path)
        ),
        Command = cmd_memory_addr(MaybePath)
    ;
        CmdToken = token_name("cdr"),
        ArgTokens = [token_num(Repetitions) | TokenPath],
        list.duplicate(Repetitions, TokenPath, DupTokenPath),
        list.condense(DupTokenPath, RepeatedTokenPath),
        parse_path(RepeatedTokenPath, RepeatedPath),
        Command = cmd_cd_path(RepeatedPath)
    ;
        ( CmdToken = token_name("cd")
        ; CmdToken = token_up
        ),
        (
            ArgTokens = [_ | _],
            parse_path(ArgTokens, Path),
            Command = cmd_cd_path(Path)
        ;
            ArgTokens = [],
            Command = cmd_cd_no_path
        )
    ;
        CmdToken = token_name("pwd"),
        ArgTokens = [],
        Command = cmd_pwd
    ;
        (
            CmdToken = token_name("track"),
            AssertInvalid = no_assert_invalid
        ;
            CmdToken = token_name("t"),
            AssertInvalid = no_assert_invalid
        ;
            CmdToken = token_name("mark"),
            AssertInvalid = assert_invalid
        ;
            CmdToken = token_name("m"),
            AssertInvalid = assert_invalid
        ),
        (
            ArgTokens = [],
            HowTrack = track_fast,
            MaybePath = no
        ;
            ArgTokens = [HeadArgToken | TailArgTokens],
            ( if
                ( HeadArgToken = token_arg("accurate")
                ; HeadArgToken = token_arg("a")
                )
            then
                HowTrack = track_accurate,
                (
                    TailArgTokens = [],
                    MaybePath = no
                ;
                    TailArgTokens = [_ | _],
                    parse_path(TailArgTokens, Path),
                    MaybePath = yes(Path)
                )
            else
                HowTrack = track_fast,
                parse_path(ArgTokens, Path),
                MaybePath = yes(Path)
            )
        ),
        Command = cmd_track(HowTrack, AssertInvalid, MaybePath)
    ;
        CmdToken = token_name("mode"),
        (
            ArgTokens = [_ | _],
            parse_path(ArgTokens, Path),
            Command = cmd_mode_query(Path)
        ;
            ArgTokens = [],
            Command = cmd_mode_query_no_path
        )
    ;
        CmdToken = token_name("format"),
        (
            ArgTokens = [],
            FormatCmd = print_params
        ;
            ArgTokens = [_ | _],
            MaybeArgWords = yes(ArgWords),
            OptionOps = option_ops_multi(short_format_cmd_option,
                long_format_cmd_option, format_cmd_option_defaults),
            getopt.process_options(OptionOps, ArgWords,
                RemainingWords, MaybeOptionTable0),
            MaybeOptionTable =
                convert_to_maybe_option_table(MaybeOptionTable0),
            lexer_words(RemainingWords, RemainingTokens),
            parse_format(RemainingTokens, Setting),
            FormatCmd = format(MaybeOptionTable, Setting)
        ),
        Command = cmd_param(FormatCmd)
    ;
        (
            CmdToken = token_name("depth"),
            ParamCmd = param_depth
        ;
            CmdToken = token_name("size"),
            ParamCmd = param_size
        ;
            CmdToken = token_name("width"),
            ParamCmd = param_width
        ;
            CmdToken = token_name("lines"),
            ParamCmd = param_lines
        ),
        (
            ArgTokens = [],
            FormatCmd = print_params
        ;
            ArgTokens = [_ | _],
            MaybeArgWords = yes(ArgWords),
            OptionOps = option_ops_multi(short_format_param_cmd_option,
                long_format_param_cmd_option,
                format_param_cmd_option_defaults),
            getopt.process_options(OptionOps, ArgWords,
                RemainingWords, MaybeOptionTable0),
            MaybeOptionTable =
                convert_to_maybe_option_table(MaybeOptionTable0),
            lexer_words(RemainingWords, RemainingTokens),
            RemainingTokens = [token_num(N)],
            param_cmd_to_setting(ParamCmd, N, Setting),
            FormatCmd = format_param(MaybeOptionTable, Setting)
        ),
        Command = cmd_param(FormatCmd)
    ;
        CmdToken = token_lessthan,
        ArgTokens = [token_num(Depth)],
        OptionOps = option_ops_multi(short_format_param_cmd_option,
            long_format_param_cmd_option, format_param_cmd_option_defaults),
        getopt.process_options(OptionOps, [], _, MaybeOptionTable0),
        MaybeOptionTable = convert_to_maybe_option_table(MaybeOptionTable0),
        FormatCmd = format_param(MaybeOptionTable, setting_depth(Depth)),
        Command = cmd_param(FormatCmd)
    ;
        CmdToken = token_name("num_io_actions"),
        ArgTokens = [token_num(N)],
        Command = cmd_param(num_io_actions(N))
    ;
        CmdToken = token_name("params"),
        Command = cmd_param(print_params)
    ;
        ( CmdToken = token_name("help")
        ; CmdToken = token_name("h")
        ; CmdToken = token_question
        ),
        ArgTokens = [],
        Command = cmd_help
    ;
        CmdToken = token_name("quit"),
        ArgTokens = [],
        Command = cmd_quit
    ).

:- pred param_cmd_to_setting(format_param_cmd::in, int::in, setting::out)
    is det.

param_cmd_to_setting(param_depth, N, setting_depth(N)).
param_cmd_to_setting(param_size,  N, setting_size(N)).
param_cmd_to_setting(param_width, N, setting_width(N)).
param_cmd_to_setting(param_lines, N, setting_lines(N)).

    % SICStus is forgiving in the syntax of paths, hence so are we.
    % XXX: Be less forgiving?
    %
:- pred parse_path(list(token)::in, path::out) is semidet.

parse_path([Token | Tokens], Path) :-
    ( if Token = token_slash then
        Path = root_rel(Dirs),
        parse_up_down_dirs(Tokens, Dirs)
    else
        Path = dot_rel(Dirs),
        parse_up_down_dirs([Token | Tokens], Dirs)
    ).

:- pred parse_up_down_dirs(list(token)::in, list(up_down_dir)::out) is semidet.

parse_up_down_dirs([], []).
parse_up_down_dirs([Token | Tokens], Dirs) :-
    (
        Token = token_num(Subdir),
        Dirs = [updown_child_num(Subdir) | RestDirs],
        parse_up_down_dirs(Tokens, RestDirs)
    ;
        Token = token_name(NamedSubdir),
        Dirs = [updown_child_name(NamedSubdir) | RestDirs],
        parse_up_down_dirs(Tokens, RestDirs)
    ;
        Token = token_dot_dot,
        Dirs = [updown_parent | RestDirs],
        parse_up_down_dirs(Tokens, RestDirs)
    ;
        % We can effectively ignore slashes (for Unix-style pathnames)
        % and carets (for SICStus-style pathnames),
        % but anything else is not allowed.
        Token = token_slash,
        parse_up_down_dirs(Tokens, Dirs)
    ;
        Token = token_up,
        parse_up_down_dirs(Tokens, Dirs)
    ).

:- pred parse_format(list(token)::in, setting::out) is semidet.

parse_format([Token], Setting) :-
    Token = token_name(TokenName),
    (
        TokenName = "flat",
        Setting = setting_format(flat)
    ;
        TokenName = "raw_pretty",
        Setting = setting_format(raw_pretty)
    ;
        TokenName = "verbose",
        Setting = setting_format(verbose)
    ;
        TokenName = "pretty",
        Setting = setting_format(pretty)
    ).

:- pred parse_format_param(list(token)::in, setting::out) is semidet.
% XXX Why is this here? It is not used.
:- pragma consider_used(pred(parse_format_param/2)).

parse_format_param([Token | Tokens], Setting) :-
    Token = token_name(TokenName),
    (
        TokenName = "depth",
        Tokens = [token_num(Depth)],
        Setting = setting_depth(Depth)
    ;
        TokenName = "size",
        Tokens = [token_num(Size)],
        Setting = setting_size(Size)
    ;
        TokenName = "width",
        Tokens = [token_num(X)],
        Setting = setting_width(X)
    ;
        TokenName = "lines",
        Tokens = [token_num(Y)],
        Setting = setting_lines(Y)
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
% show_command(cd_path(Path)) -->
%   io.write_string("cd "),
%   show_path(Path),
%   io.nl.
% show_command(cd_no_path) -->
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
:- end_module mdb.parse.
%---------------------------------------------------------------------------%
