%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2015, 2017 The University of Melbourne.
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: shell_util.m.
% Main author: stayl.
%
% Utilities for interacting with the shell.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module libs.shell_util.
:- interface.

    % Quote an argument to a shell command.
    %
:- func quote_shell_cmd_arg(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

quote_shell_cmd_arg(Arg0) = Arg :-
    % XXX Instead of using dir.use_windows_paths, this should really
    % test whether we are using a Unix or Windows shell.
    ( if dir.use_windows_paths then
        ( if
            ( string.contains_match(char.is_whitespace, Arg0)
            ; Arg0 = ""
            )
        then
            Arg = """" ++ Arg0 ++ """"
        else
            Arg = Arg0
        )
    else
        ArgList = quote_arg_unix(string.to_char_list(Arg0)),
        (
            ArgList = [],
            Arg = """"""
        ;
            ArgList = [_ | _],
            ( if
                list.member(Char, ArgList),
                not
                    ( char.is_alnum_or_underscore(Char)
                    ; Char = ('-')
                    ; Char = ('/')
                    ; Char = ('.')
                    ; Char = (',')
                    ; Char = (':')
                    )
            then
                Arg = """" ++ string.from_char_list(ArgList) ++ """"
            else
                Arg = string.from_char_list(ArgList)
            )
        )
    ).

:- func quote_arg_unix(list(char)) = list(char).

quote_arg_unix([]) = [].
quote_arg_unix([Char | Chars0]) = Chars :-
    Chars1 = quote_arg_unix(Chars0),
    ( if quote_char_unix(Char) then
        Chars = [('\\'), Char | Chars1]
    else
        Chars = [Char | Chars1]
    ).

:- pred quote_char_unix(char::in) is semidet.

quote_char_unix('\\').
quote_char_unix('"').
quote_char_unix('`').
quote_char_unix('$').

%-----------------------------------------------------------------------------%
:- end_module libs.shell_util.
%-----------------------------------------------------------------------------%
