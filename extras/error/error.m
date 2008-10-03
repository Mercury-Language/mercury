%------------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%------------------------------------------------------------------------------%
% Copyright (C) 2000,2003, 2006-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% Author: Tom Conway <conway@cs.mu.oz.au>
%
% Modifications by: Tyson Dowd <trd@cs.mu.oz.au>
%
% error: a tool like the old unix tool of the same name.
%
% error will process compiler error message, and insert them into source
% files as comments.  This means you can fix a bunch of errors all in a
% single editing session.
%
% error takes a list of files on the command line, and looks for errors
% in the common format:
%
% filename:linenumber: error message
%
% for example:
%
% foo.m:041: In clause for `main(di, uo)':
% foo.m:041:   mode mismatch in disjunction.
% foo.m:041:   `Errors' :: free, ground.
%
% error will then insert the error message at the appropriate line in
% the file, with a ### preceding the error message, for example:
%
% /* ###  In clause for `main(di, uo)': */
% /* ###    mode mismatch in disjunction. */
% /* ###    `Errors' :: free, ground. */
% main -->
%
% Most compilers will output in this format (for example Mercury outputs
% error messages in this format, so does gcc).
%
% If the -v option is given, error will first insert the error messages,
% and then invoke your editor on the list of files which contained errors.
% error looks in the environment variable EDITOR for your editor, and if
% that isn't found, it will attempt to use "vim".
%
% possible improvements:
%   - better error handling
%   - look for variables other than EDITOR
%   - usage message, help message
%   - take input from stdin if no filenames given or if - is a
%     filename
%   - handle options using getopt
%   - handle other commenting styles than /* .... */

:- module error.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- type file_error_map == map(line_number, list(message)).
:- type error_map == map(filename, file_error_map).
:- type filename == string.
:- type line_number == int.
:- type message == string.

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    ( Args0 = ["-v" | Args1] ->
        InvokeEditor = yes,
        Args = Args1
    ;
        InvokeEditor = no,
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
        process_error_map(ErrorMap, !IO),
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
    io.open_input(FileName, Result, !IO),
    (
        Result = ok(Stream),
        read_errors(Stream, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO),
        io.close_input(Stream, !IO),
        read_error_files(FileNames, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        string.format("error: %s\n", [s(ErrorMsg)], ProblemMsg),
        !:ProblemMsgs = !.ProblemMsgs ++ [ProblemMsg]
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
        ( parse_error(Chars, File, Line, Message) ->
            insert_error(File, Line, Message, !ErrorMap)
        ;
            string.from_char_list(Chars, Str),
            string.format("ignoring: %s", [s(Str)], IgnoreMsg),
            !:IgnoreMsgs = !.IgnoreMsgs ++ [IgnoreMsg]
        ),
        read_errors(Stream, !ErrorMap, !ProblemMsgs, !IgnoreMsgs, !IO)
    ).

:- pred insert_error(filename::in, line_number::in, message::in,
    error_map::in, error_map::out) is det.

insert_error(FileName, LineNumber, Message, ErrorMap0, ErrorMap) :-
    ( map.search(ErrorMap0, FileName, FileMap0) ->
        ( map.search(FileMap0, LineNumber, Messages0) ->
            Messages = Messages0 ++ [Message]
        ;
            Messages = [Message]
        ),
        set(FileMap0, LineNumber, Messages, FileMap),
        map.det_update(ErrorMap0, FileName, FileMap, ErrorMap)
    ;
        map.from_assoc_list([LineNumber - [Message]], FileMap),
        map.det_insert(ErrorMap0, FileName, FileMap, ErrorMap)
    ).

:- pred parse_error(list(char)::in, filename::out, line_number::out,
    message::out) is semidet.

parse_error(Chars, File, Line, Message) :-
    takewhile((pred(C0::in) is semidet :-
        C0 \= (':')
    ), Chars, FileChars, [_|Rest0]),
    takewhile((pred(C1::in) is semidet :-
        C1 \= (':')
    ), Rest0, LineChars, [_|Rest1]), % throw away the :
    takewhile((pred(C2::in) is semidet :-
        C2 \= ('\n')
    ), Rest1, MsgChars, _),
    string.from_char_list(FileChars, File),
    string.from_char_list(LineChars, LineStr),
    string.to_int(LineStr, Line),
    string.from_char_list(MsgChars, Message).

%------------------------------------------------------------------------------%

:- pred process_error_map(error_map::in, io::di, io::uo) is det.

process_error_map(ErrorMap, !IO) :-
    map.to_assoc_list(ErrorMap, ErrorList),
    process_error_map_2(ErrorList, !IO).

:- pred process_error_map_2(assoc_list(filename, file_error_map)::in,
    io::di, io::uo) is det.

process_error_map_2([], !IO).
process_error_map_2([Head | Tail], !IO) :-
    Head = FileName - FileErrorMap,
    map.to_assoc_list(FileErrorMap, FileErrorList),
    string.append(FileName, ".orig", OrigFileName),
    io.rename_file(FileName, OrigFileName, RenameResult, !IO),
    (
        RenameResult = ok,
        io.open_input(OrigFileName, InputResult, !IO),
        (
            InputResult = ok(InputStream),
            io.open_output(FileName, Res2, !IO),
            (
                Res2 = ok(OutputStream),
                merge_file(InputStream, OutputStream, FileErrorList, 1, !IO),
                io.close_output(OutputStream, !IO),
                % There is nothing we can do if the remove fails.
                io.remove_file(OrigFileName, _RemoveResult, !IO),

                % progress message.
                io.format("updated %s\n", [s(FileName)], !IO)
            ;
                Res2 = error(Err),
                io.error_message(Err, Msg),
                io.stderr_stream(StdErr, !IO),
                io.format(StdErr, "error: %s\n", [s(Msg)], !IO),
                io.rename_file(OrigFileName, FileName, _, !IO)
            ),
            io.close_input(InputStream, !IO)
        ;
            InputResult = error(Error),
            io.error_message(Error, ErrorMsg),
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "error: %s\n", [s(ErrorMsg)], !IO),
            % There is nothing we can do if the rename fails.
            io.rename_file(OrigFileName, FileName, _RenameResult, !IO)
        )
    ;
        RenameResult = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "error: cannot rename file: %s.\n", [s(ErrorMsg)],
            !IO)
    ),
    process_error_map_2(Tail, !IO).

:- pred merge_file(io.input_stream::in, io.output_stream::in,
    assoc_list(line_number, list(message))::in, int::in, io::di, io::uo)
    is det.

merge_file(InputStream, OutputStream, [], _, !IO) :-
    copy_rest(InputStream, OutputStream, !IO).
merge_file(InputStream, OutputStream, [Head | Tail], CurrentLineNumber, !IO) :-
    Head = LineNumber - Msgs,
    ( LineNumber =< CurrentLineNumber ->
        CommentMsgs = list.map(make_comment, Msgs),
        list.foldl(io.write_string(OutputStream), CommentMsgs, !IO),
        merge_file(InputStream, OutputStream, Tail, CurrentLineNumber, !IO)
    ;
        io.read_line(InputStream, Res0, !IO),
        (
            Res0 = eof,
            error_rest(OutputStream, [Head | Tail], !IO)
        ;
            Res0 = error(Err),
            io.error_message(Err, Msg),
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "error: %s\n", [s(Msg)], !IO)
        ;
            Res0 = ok(Chars),
            string.from_char_list(Chars, Str),
            io.write_string(OutputStream, Str, !IO),
            merge_file(InputStream, OutputStream,
                [Head | Tail], CurrentLineNumber + 1, !IO)
        )
    ).

:- pred copy_rest(io.input_stream::in, io.output_stream::in, io::di, io::uo)
    is det.

copy_rest(InputStream, OutputStream, !IO) :-
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
        copy_rest(InputStream, OutputStream, !IO)
    ).

:- pred error_rest(io.output_stream::in,
    assoc_list(line_number, list(message))::in, io::di, io::uo) is det.

error_rest(_OutputStream, [], !IO).
error_rest(OutputStream, [Head | Tail], !IO) :-
    Head = _LineNumber - Msgs,
    CommentMsgs = list.map(make_comment, Msgs),
    list.foldl(io.write_string(OutputStream), CommentMsgs, !IO),
    error_rest(OutputStream, Tail, !IO).

:- func make_comment(string) = string.

make_comment(Msg) = "/* ### " ++ Msg ++ " */\n".

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
