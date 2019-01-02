%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: bromage
% Simplified by Marnix Klooster <marnix@worldonline.nl>
%
% This module provides file input. One can read a file entirely,
% select a single line from a read file, get the number of lines
% in a read file, and convert a read file to a list of strings.
%
% Every file has a filename attached to it.
%
%-----------------------------------------------------------------------------%

:- module file.
:- interface.

%-----------------------------------------------------------------------------%

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type file.

    % read_file reads a file from a filename.
    %
:- pred read_file(string::in, io.res(file)::out, io::di, io::uo) is det.

    % read_input reads a file from the input stream.
    %
:- pred read_input(string::in, io.res(file)::out, io::di, io::uo) is det.

    % get_line retrieves a line from a file.
    % (Lines are numbered from 0.)
    % Fails if the line is out of bounds.
    %
:- pred get_line(file::in, int::in, string::out) is semidet.

    % get_numlines returns the number of lines in a file.
    %
:- pred get_numlines(file::in, int::out) is det.

    % from_list converts a list of lines to a file.
    %
:- pred from_list(string::in, list(string)::in, file::out) is det.

    % to_list converts a file into a list of lines.
    %
:- pred to_list(file::in, list(string)::out) is det.

    % get_file_name returns the name of the file.
    %
:- pred get_file_name(file::in, string::out) is det.

    % set_file_name sets the name of the file.
    %
:- pred set_file_name(file::in, string::in, file::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type file
    --->    file(
                file_name     :: string,
                file_contents :: array(string)
            ).

read_file(FileName, File, !IO) :-
    % Open the stream, read from the stream, then close the stream.
    io.open_input(FileName, Res, !IO),
    (
        Res = ok(InputStream),
        file.read_stream(InputStream, Contents, !IO),
        io.close_input(InputStream, !IO),
        File = ok(file(FileName, Contents))
    ;
        Res = error(Error),
        File = error(Error)
    ).

read_input(FileName, ok(file(FileName, Contents)), !IO) :-
    % Get the input stream, then read from it.
    io.input_stream(InputStream, !IO),
    file.read_stream(InputStream, Contents, !IO).

    % read_stream is the "real" file reader.
    %
:- pred read_stream(io.input_stream::in, array(string)::array_uo,
    io::di, io::uo) is det.

read_stream(Stream, File, !IO) :-
    file.read_stream2(Stream, 0, File, !IO).

    % Given a Stream from which LinesIn lines have already been read,
    % fill File[LinesIn] to File[LinesOut-1] with the rest of the lines.
    % LinesOut is the number of lines in the file.
    % (Note that line numbering starts at zero.)
    %
:- pred read_stream2(io.input_stream::in, int::in,
    array(string)::array_uo, io::di, io::uo) is det.

read_stream2(Stream, LineNo, File, !IO) :-
    io.read_line_as_string(Stream, Res, !IO),
    (
        Res = eof,
        array.init(LineNo, "", File)
    ;
        Res = ok(Line),
        file.read_stream2(Stream, LineNo + 1, File0, !IO),
        array.set(LineNo, Line, File0, File)
    ;
        Res = error(Error),
        io.error_message(Error, Msg),
        error(Msg)
    ).

%-----------------------------------------------------------------------------%

get_line(file(_, Contents), LineNo, Line) :-
    array.semidet_lookup(Contents, LineNo, Line).

get_numlines(file(_, Contents), NumLines1 + 1) :-
    array.bounds(Contents, _, NumLines1).

%-----------------------------------------------------------------------------%

to_list(file(_, Contents), List) :-
    array.to_list(Contents, List).

from_list(FileName, List, file(FileName, Contents)) :-
    array.from_list(List, Contents).

%-----------------------------------------------------------------------------%

get_file_name(file(FileName, _), FileName).

set_file_name(file(_, B), FileName, file(FileName, B)).

%-----------------------------------------------------------------------------%
:- end_module file.
%-----------------------------------------------------------------------------%
