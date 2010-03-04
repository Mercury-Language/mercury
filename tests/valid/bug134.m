%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Compiling the following with rotd-2010-03-03 and before using
%
%    $ mmc --grade hlc.gc --no-optimize-dead-procs --no-static-ground-terms
%
% yields:
%
%    Software Error: map.lookup: key not found
%	    Key Type: term.var(parse_tree.prog_data.prog_var_type)
%	    Key Value: var(2)
%	    Value Type: ml_backend.ml_gen_info.ml_ground_term
%    Stack dump not available in this grade.
%
% (This test is derived from cut down version of browser/parse.m.)
%
%---------------------------------------------------------------------------%

:- module bug134.
:- interface.

:- import_module getopt.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type command ---> cmd_unknown.
:- type path ---> path.
:- type dir ---> dir.

:- type format_option
    --->    flat
    ;       pretty.

:- pred read_command_external(command::out, io::di, io::uo) is det.

:- pred parse(list(string)::in, command::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module string.

:- type token
    --->    token_dot
    ;       token_unknown(char).

read_command_external(Command, !IO) :-
    io.read(Result, !IO),
    (
        Result = ok(StringToParse),
        string.words_separator(char.is_whitespace, StringToParse) = Words,
        ( parse(Words, CommandPrime) ->
            Command = CommandPrime
        ;
            Command = cmd_unknown
        )
    ;
        Result = eof,
        Command = cmd_unknown
    ;
        Result = error(_, _),
        Command = cmd_unknown
    ).

:- pred lexer_words(list(string)::in, list(token)::out) is det.

lexer_words(_, []).

:- pred lexer_word(string::in, list(token)::out) is det.

lexer_word(_, []).

%---------------------------------------------------------------------------%

parse(Words, Command) :-
    (
        Words = [],
        Command = cmd_unknown
    ;
        Words = [CmdWord | ArgWords],
        lexer_word(CmdWord, CmdTokens),
        lexer_words(ArgWords, ArgTokens),
        list.append(CmdTokens, ArgTokens, AllTokens),
        AllTokens = [FirstToken | LaterTokens],
        parse_cmd(FirstToken, LaterTokens, Command)
    ).

:- pred parse_cmd(token::in, list(token)::in, command::out) is semidet.

parse_cmd(_CmdToken, _ArgTokens, cmd_unknown) :-
    semidet_false.

%---------------------------------------------------------------------------%

:- pred format_option_defaults(format_option::out, option_data::out) is multi.

format_option_defaults(flat,        bool(no)).
format_option_defaults(pretty,      bool(no)).

%---------------------------------------------------------------------------%
