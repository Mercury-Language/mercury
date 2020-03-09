%------------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%------------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% Author: Zoltan Somogyi
%
% split_file: a tool that helps in splitting up a file into two or more pieces.
%
% This program takes one input file and the names of two or more new files
% to be created. It considers the input file to consist of a sequence of lines,
% some of which are control lines, and rest are data lines. What distinguishes
% between them is that control lines start with the keyword SPLIT and a space
% in the first six columns. After the keyword, there should be a
% space-separated list of one or more integers in the range 1 to N,
% where N is the number of output files. This tells split_file that
% the data lines between this control line and the next control line
% should be put into the output file(s) whose number(s) appear on the
% control line. The data lines before the first control line go into
% the first output file.
%
% For example,
%
%   start
%   SPLIT 1
%   123
%   abc
%   SPLIT 1 2
%   def
%   SPLIT 2
%   ghi
%
% will cause split_file to put
%
%   start
%   123
%   abc
%   def
%
% into the first output file and
%
%   def
%   ghi
%
% into the second output file.

:- module split_file.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

%------------------------------------------------------------------------------%

:- type file_name == string.
:- type output_file
    --->    output_file(
                % The name of the output file.
                file_name,

                % The lines being queued up to be printed out to that file.
                cord(string)
            ).
:- type output_map == map(int, output_file).

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    io.stderr_stream(StdErrStream, !IO),
    ( if
        Args = [InputFileName | OutputFileNames],
        list.length(OutputFileNames) > 1
    then
        io.open_input(InputFileName, OpenInputResult, !IO),
        (
            OpenInputResult = ok(InputStream),
            io.read_file_as_string(InputStream, ReadInputResult, !IO),
            (
                ReadInputResult = ok(InputFileString),
                make_output_map(OutputFileNames, 1, map.init, OutputMap0),
                InputLines = string.split_at_char('\n', InputFileString),
                split_file(InputLines, InputFileName, 1, [1],
                    OutputMap0, OutputMap, cord.init, ErrorsCord),
                Errors = cord.list(ErrorsCord),
                (
                    Errors = [],
                    map.to_assoc_list(OutputMap, OutputAssocList),
                    output_split_files(StdErrStream, OutputAssocList, !IO)
                ;
                    Errors = [_ | _],
                    list.foldl(io.write_string(StdErrStream), Errors, !IO)
                )
            ;
                ReadInputResult = error(_, ReadInputError),
                io.error_message(ReadInputError, ReadInputErrorMsg),
                io.format(StdErrStream, "error reading %s: %s\n",
                    [s(InputFileName), s(ReadInputErrorMsg)], !IO),
                io.set_exit_status(1, !IO)
            )
        ;
            OpenInputResult = error(OpenInputError),
            io.error_message(OpenInputError, OpenInputErrorMsg),
            io.format(StdErrStream, "error opening %s for reading: %s\n",
                [s(InputFileName), s(OpenInputErrorMsg)], !IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.write_string(StdErrStream, "usage: split_file input_file " ++
            "output_file1 output_file2 [...]\n", !IO),
        io.set_exit_status(1, !IO)
    ).

%------------------------------------------------------------------------------%

:- pred make_output_map(list(file_name)::in, int::in,
    output_map::in, output_map::out) is det.

make_output_map([], _, !OutputMap).
make_output_map([OutputFileName | OutputFileNames], FileNumber, !OutputMap) :-
    OutputFile = output_file(OutputFileName, cord.init),
    map.det_insert(FileNumber, OutputFile, !OutputMap),
    make_output_map(OutputFileNames, FileNumber + 1, !OutputMap).

%------------------------------------------------------------------------------%

:- pred split_file(list(string)::in, file_name::in, int::in,
    list(int)::in, output_map::in, output_map::out,
    cord(string)::in, cord(string)::out) is det.

split_file([], _, _, _, !OutputMap, !Errors).
split_file([Line | Lines], FileName, LineNumber, !.CurOutputs,
        !OutputMap, !Errors) :-
    ( if Line = "", Lines = [] then
        % When we split up the contents of the input file with split_at_char,
        % the char being a newline, split_at_char will return a string
        % *after* the last newline, even if it is empty. We could delete it
        % immediately after the call to split_at_char, but that would
        % add an extra traversal of the line list. Instead, we ignore it here.
        true
    else if string.remove_prefix("SPLIT ", Line, RestOfCtrlLine) then
        RestOfCtrlLineWords =
            string.split_at_separator(char.is_whitespace, RestOfCtrlLine),
        parse_control_line_numbers(RestOfCtrlLineWords,
            FileName, LineNumber, !.OutputMap, [], !:CurOutputs, !Errors)
    else
        record_data_line(Line, !.CurOutputs, !OutputMap)
    ),
    split_file(Lines, FileName, LineNumber + 1, !.CurOutputs,
        !OutputMap, !Errors).

:- pred parse_control_line_numbers(list(string)::in,
    file_name::in, int::in, output_map::in,
    list(int)::in, list(int)::out, cord(string)::in, cord(string)::out) is det.

parse_control_line_numbers([], _, _, _, !Outputs, !Errors).
parse_control_line_numbers([Word | Words], FileName, LineNumber,
        OutputMap, !Outputs, !Errors) :-
    ( if string.to_int(Word, Num) then
        ( if map.search(OutputMap, Num, _) then
            % The order of the numbers does not matter.
            !:Outputs = [Num | !.Outputs]
        else
            string.format("%s:%d: %s is not a valid output file number\n",
                [s(FileName), i(LineNumber), s(Word)], Error),
            cord.snoc(Error, !Errors)
        )
    else
        string.format("%s:%d: %s is not a number\n",
            [s(FileName), i(LineNumber), s(Word)], Error),
        cord.snoc(Error, !Errors)
    ),
    parse_control_line_numbers(Words, FileName, LineNumber,
        OutputMap, !Outputs, !Errors).

:- pred record_data_line(string::in, list(int)::in,
    output_map::in, output_map::out) is det.

record_data_line(_, [], !OutputMap).
record_data_line(DataLine, [Output | Outputs], !OutputMap) :-
    map.lookup(!.OutputMap, Output, OutputFile0),
    OutputFile0 = output_file(OutputFileName, DataLinesCord0),
    cord.snoc(DataLine, DataLinesCord0, DataLinesCord),
    OutputFile = output_file(OutputFileName, DataLinesCord),
    map.det_update(Output, OutputFile, !OutputMap),
    record_data_line(DataLine, Outputs, !OutputMap).

%------------------------------------------------------------------------------%

:- pred output_split_files(io.text_output_stream::in,
    assoc_list(int, output_file)::in, io::di, io::uo) is det.

output_split_files(_, [], !IO).
output_split_files(StdErrStream, [_Output - OutputFile | OutputFiles], !IO) :-
    OutputFile = output_file(OutputFileName, DataLinesCord),
    io.open_output(OutputFileName, OpenOutputResult, !IO),
    (
        OpenOutputResult = ok(OutputStream),
        DataLines = cord.list(DataLinesCord),
        list.foldl(write_line_and_nl(OutputStream), DataLines, !IO),
        io.close_output(OutputStream, !IO)
    ;
        OpenOutputResult = error(OpenOutputError),
        io.error_message(OpenOutputError, OpenOutputErrorMsg),
        io.format(StdErrStream, "error opening %s for writing: %s\n",
            [s(OutputFileName), s(OpenOutputErrorMsg)], !IO),
        io.set_exit_status(1, !IO)
    ),
    output_split_files(StdErrStream, OutputFiles, !IO).

:- pred write_line_and_nl(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

write_line_and_nl(OutputStream, Line, !IO) :-
    io.format(OutputStream, "%s\n", [s(Line)], !IO).

%------------------------------------------------------------------------------%
