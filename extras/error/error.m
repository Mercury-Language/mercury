%------------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%------------------------------------------------------------------------------%
% Copyright (C) 2000,2003, 2006-2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% Author: Tom Conway <conway@cs.mu.oz.au>.
% Modifications by Tyson Dowd <trd@cs.mu.oz.au>
% and Zoltan Somogyi (zs@cs.mu.OZ.AU).
%
% Error: a tool like the old unix tool of the same name.
%
% Error's job is to parse compiler error messages, and insert each message
% as a comment into its source file next to the line the message refers to.
% By making the error messages move with the code they refer to as
% the code is changed, this can help programmers fix many errors in a
% single editing session; after fixing one error, they don't have to recompile
% to get updated line numbers on the remaining errors.
%
% Error takes a list of files on the command line, and looks for messages
% in the format:
%
%   filename:linenumber: error message
%
% Most compilers output error messages in this format; gcc does, and the
% Mercury compiler does too. This is an example from the Mercury compiler:
%
% foo.m:041: In clause for `main(di, uo)':
% foo.m:041:   mode mismatch in disjunction.
% foo.m:041:   `Errors' :: free, ground.
%
% When such an error message occurs in a file that is input to error,
% error will insert the error message at the appropriate line in the file,
% with a ### marker preceding each line of the error message, like this:
%
% /* ###  In clause for `main(di, uo)': */
% /* ###    mode mismatch in disjunction. */
% /* ###    `Errors' :: free, ground. */
% main(!IO) :-
%
% If the -v option is given, error will first insert the error messages,
% and then invoke your editor on the list of files which contained errors.
% error looks in the environment variable EDITOR for your editor, and if
% that isn't found, it will attempt to use "vim".
%
% Possible improvements:
%   - better error handling
%   - look for environment variables other than EDITOR
%   - usage message, help message
%   - handle commenting styles other than /* .... */ and %

:- module error.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- type filename == string.
:- type line_number == int.
:- type message == string.
:- type file_error_map == map(line_number, list(message)).
:- type error_map == map(filename, file_error_map).

:- type comment_style
    --->    cs_slash_star
    ;       cs_percent.

%------------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_options, long_options, option_defaults),
    getopt.process_options(OptionOps, Args0, Args, GetoptResult),
    (
        GetoptResult = error(ErrorMessage),
        io.write_string(ErrorMessage, !IO),
        io.set_exit_status(1, !IO)
    ;
        GetoptResult = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, invoke_editor, InvokeEditor),
        getopt.lookup_bool_option(OptionTable, comment_slash_star, SlashStar),
        getopt.lookup_bool_option(OptionTable, comment_percent, Percent),
        (
            SlashStar = no,
            Percent = no,
            MaybeStyle = yes(cs_slash_star)
        ;
            SlashStar = no,
            Percent = yes,
            MaybeStyle = yes(cs_percent)
        ;
            SlashStar = yes,
            Percent = no,
            MaybeStyle = yes(cs_slash_star)
        ;
            SlashStar = yes,
            Percent = yes,
            io.write_string("error: you can specify only one comment style\n",
                !IO),
            io.set_exit_status(1, !IO),
            MaybeStyle = no
        ),
        (
            MaybeStyle = no
        ;
            MaybeStyle = yes(Style),
            run_error(InvokeEditor, Style, Args, !IO)
        )
    ).

:- pred run_error(bool::in, comment_style::in, list(string)::in,
    io::di, io::uo) is det.

run_error(InvokeEditor, Style, Args0, !IO) :-
    % If there are no filename arguments, default to processing standard input.
    (
        Args0 = [],
        Args = ["-"]
    ;
        Args0 = [_ | _],
        Args = Args0
    ),
    map.init(ErrorMap0),
    read_error_files(Args, ErrorMap0, ErrorMap, [], ProblemMsgs,
        [], IgnoreMsgs, !IO),
    io.stderr_stream(StdErr, !IO),
    (
        ProblemMsgs = [_ | _],
        list.foldl(io.write_string(StdErr), ProblemMsgs, !IO)
    ;
        ProblemMsgs = [],
        % Print out all the lines in error files we couldn't parse.
        list.foldl(io.write_string(StdErr), IgnoreMsgs, !IO),

        % Insert all the error lines we COULD parse into the files they name.
        process_error_map(Style, ErrorMap, !IO),
        (
            InvokeEditor = yes,
            invoke_editor(ErrorMap, !IO)
        ;
            InvokeEditor = no
        )
    ).

%------------------------------------------------------------------------------%

:- pred read_error_files(list(string)::in, error_map::in, error_map::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

read_error_files([], !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO).
read_error_files([FileName | FileNames], !ErrorMap, !ProblemMsgs, !IgnoreMsgs,
        !IO) :-
    ( if FileName = "-" then
        InputStream = io.stdin_stream,
        read_errors(InputStream, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO),
        read_error_files(FileNames, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO)
    else
        io.open_input(FileName, Result, !IO),
        (
            Result = ok(InputStream),
            read_errors(InputStream, !ErrorMap, !ProblemMsgs, !IgnoreMsgs,
                !IO),
            io.close_input(InputStream, !IO),
            read_error_files(FileNames, !ErrorMap, !ProblemMsgs, !IgnoreMsgs,
                !IO)
        ;
            Result = error(Error),
            io.error_message(Error, ErrorMsg),
            string.format("error: %s\n", [s(ErrorMsg)], ProblemMsg),
            !:ProblemMsgs = !.ProblemMsgs ++ [ProblemMsg]
        )
    ).

:- pred read_errors(io.input_stream::in, error_map::in, error_map::out,
    list(string)::in, list(string)::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

read_errors(Stream, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO) :-
    io.read_line(Stream, Result, !IO),
    (
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        string.format("error: %s\n", [s(ErrorMsg)], ProblemMsg),
        !:ProblemMsgs = !.ProblemMsgs ++ [ProblemMsg]
    ;
        Result = ok(Chars),
        ( if parse_error(Chars, File, Line, Message) then
            insert_error(File, Line, Message, !ErrorMap)
        else
            string.from_char_list(Chars, Str),
            string.format("ignoring: %s", [s(Str)], IgnoreMsg),
            !:IgnoreMsgs = !.IgnoreMsgs ++ [IgnoreMsg]
        ),
        read_errors(Stream, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO)
    ).

:- pred insert_error(filename::in, line_number::in, message::in,
    error_map::in, error_map::out) is det.

insert_error(FileName, LineNumber, Message, !ErrorMap) :-
    ( if map.search(!.ErrorMap, FileName, FileMap0) then
        ( if map.search(FileMap0, LineNumber, Messages0) then
            Messages = Messages0 ++ [Message]
        else
            Messages = [Message]
        ),
        map.set(LineNumber, Messages, FileMap0, FileMap),
        map.det_update(FileName, FileMap, !ErrorMap)
    else
        map.from_assoc_list([LineNumber - [Message]], FileMap),
        map.det_insert(FileName, FileMap, !ErrorMap)
    ).

:- pred parse_error(list(char)::in, filename::out, line_number::out,
    message::out) is semidet.

parse_error(Chars, File, Line, Message) :-
    NotColon =
        ( pred(C::in) is semidet :-
            C \= (':')
        ),
    NotNewLine =
        ( pred(C::in) is semidet :-
            C \= ('\n')
        ),
    % The _s throw away the colons after the filename and line number.
    list.take_while(NotColon, Chars, FileChars, [_ | AfterFileChars]),
    list.take_while(NotColon, AfterFileChars, LineChars, [_ | AfterLineChars]),
    list.take_while(NotNewLine, AfterLineChars, MsgChars, _),
    string.from_char_list(FileChars, File),
    string.from_char_list(LineChars, LineStr),
    string.to_int(LineStr, Line),
    string.from_char_list(MsgChars, Message).

%------------------------------------------------------------------------------%

:- pred process_error_map(comment_style::in, error_map::in,
    io::di, io::uo) is det.

process_error_map(Style, ErrorMap, !IO) :-
    map.to_assoc_list(ErrorMap, ErrorAssocList),
    list.foldl(process_errors_for_file(Style), ErrorAssocList, !IO).

:- pred process_errors_for_file(comment_style::in,
    pair(filename, file_error_map)::in, io::di, io::uo) is det.

process_errors_for_file(Style, FileName - FileErrorMap, !IO) :-
    map.to_assoc_list(FileErrorMap, FileErrors),
    string.append(FileName, ".orig", DotOrigFileName),
    io.rename_file(FileName, DotOrigFileName, RenameResult, !IO),
    (
        RenameResult = ok,
        io.open_input(DotOrigFileName, InputResult, !IO),
        (
            InputResult = ok(InputStream),
            io.open_output(FileName, OutputResult, !IO),
            (
                OutputResult = ok(OutputStream),
                merge_errors_into_file(InputStream, OutputStream, Style,
                    FileErrors, 1, !IO),
                io.close_output(OutputStream, !IO),
                % There is nothing we can do if the remove fails.
                io.remove_file(DotOrigFileName, _RemoveResult, !IO),

                % Print progress message.
                io.format("updated %s\n", [s(FileName)], !IO)
            ;
                OutputResult = error(Err),
                io.error_message(Err, Msg),
                io.stderr_stream(StdErr, !IO),
                io.format(StdErr, "error: %s\n", [s(Msg)], !IO),
                % Move the original file back to its original name.
                io.rename_file(DotOrigFileName, FileName, _, !IO)
            ),
            io.close_input(InputStream, !IO)
        ;
            InputResult = error(Error),
            io.error_message(Error, ErrorMsg),
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "error: %s\n", [s(ErrorMsg)], !IO),
            % There is nothing we can do if the rename fails.
            io.rename_file(DotOrigFileName, FileName, _RenameResult, !IO)
        )
    ;
        RenameResult = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "error: cannot rename file: %s.\n", [s(ErrorMsg)],
            !IO)
    ).

:- pred merge_errors_into_file(io.input_stream::in, io.output_stream::in,
    comment_style::in, assoc_list(line_number, list(message))::in, int::in,
    io::di, io::uo) is det.

merge_errors_into_file(InputStream, OutputStream, _, [], _, !IO) :-
    copy_rest_of_file(InputStream, OutputStream, !IO).
merge_errors_into_file(InputStream, OutputStream, Style, [Head | Tail],
        CurrentLineNumber, !IO) :-
    Head = LineNumber - Msgs,
    ( if LineNumber =< CurrentLineNumber then
        CommentMsgs = list.map(make_comment(Style), Msgs),
        list.foldl(io.write_string(OutputStream), CommentMsgs, !IO),
        merge_errors_into_file(InputStream, OutputStream, Style,
            Tail, CurrentLineNumber, !IO)
    else
        io.read_line(InputStream, Res0, !IO),
        (
            Res0 = eof,
            merge_rest_of_errors(OutputStream, Style, [Head | Tail], !IO)
        ;
            Res0 = error(Err),
            io.error_message(Err, Msg),
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "error: %s\n", [s(Msg)], !IO)
        ;
            Res0 = ok(Chars),
            string.from_char_list(Chars, Str),
            io.write_string(OutputStream, Str, !IO),
            merge_errors_into_file(InputStream, OutputStream, Style,
                [Head | Tail], CurrentLineNumber + 1, !IO)
        )
    ).

:- pred copy_rest_of_file(io.input_stream::in, io.output_stream::in,
    io::di, io::uo) is det.

copy_rest_of_file(InputStream, OutputStream, !IO) :-
    io.read_line(InputStream, Result, !IO),
    (
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "error: %s\n", [s(ErrorMsg)], !IO)
    ;
        Result = ok(Chars),
        string.from_char_list(Chars, Str),
        io.write_string(OutputStream, Str, !IO),
        copy_rest_of_file(InputStream, OutputStream, !IO)
    ).

:- pred merge_rest_of_errors(io.output_stream::in, comment_style::in,
    assoc_list(line_number, list(message))::in, io::di, io::uo) is det.

merge_rest_of_errors(_OutputStream, _, [], !IO).
merge_rest_of_errors(OutputStream, Style, [Head | Tail], !IO) :-
    Head = _LineNumber - Msgs,
    CommentMsgs = list.map(make_comment(Style), Msgs),
    list.foldl(io.write_string(OutputStream), CommentMsgs, !IO),
    merge_rest_of_errors(OutputStream, Style, Tail, !IO).

:- func make_comment(comment_style, string) = string.

make_comment(Style, Msg) = CommentMsg :-
    (
        Style = cs_slash_star,
        CommentMsg = "/* ### " ++ Msg ++ " */\n"
    ;
        Style = cs_percent,
        CommentMsg = "% ### " ++ Msg ++ "\n"
    ).

%------------------------------------------------------------------------------%

:- pred invoke_editor(error_map::in, io::di, io::uo) is det.

invoke_editor(ErrorMap, !IO) :-
    io.get_environment_var("EDITOR", MaybeEditor, !IO),
    Editor = (if MaybeEditor = yes(Editor0) then Editor0 else "vim" ),

    map.sorted_keys(ErrorMap, FileNames),
    AllFileNames = string.join_list(" ", FileNames),

    string.format("%s +/### %s", [s(Editor), s(AllFileNames)], CommandStr),
    % XXX We ignore the error status, which isn't nice.
    io.call_system(CommandStr, _Res, !IO).

%------------------------------------------------------------------------------%

:- type option
    --->    invoke_editor
    ;       comment_slash_star
    ;       comment_percent.

:- pred short_options(char::in, option::out) is semidet.

short_options('v', invoke_editor).
short_options('s', comment_slash_star).
short_options('p', comment_percent).

:- pred long_options(string::in, option::out) is semidet.

long_options("invoke_editor", invoke_editor).
long_options("slash-star-comments", comment_slash_star).
long_options("percent-comments", comment_percent).

:- pred option_defaults(option::out, option_data::out) is multi.

option_defaults(invoke_editor, bool(no)).
option_defaults(comment_slash_star, bool(no)).
option_defaults(comment_percent, bool(no)).
