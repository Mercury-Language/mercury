%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Simplified by Marnix Klooster <marnix@worldonline.nl>

% This module provides file input.  One can read a file entirely,
% select a single line from a read file, get the number of lines
% in a read file, and convert a read file to a list of strings.
%
% Every file has a filename attached to it.

%-----------------------------------------------------------------------------%

:- module file.
:- interface.

:- import_module io, list, string.

:- type file.

	% file__read_file reads a file from a filename.
:- pred file__read_file(string, io__res(file), io__state, io__state).
:- mode file__read_file(in, out, di, uo) is det.

	% file__read_input reads a file from the input
	% stream.
:- pred file__read_input(string, io__res(file), io__state, io__state).
:- mode file__read_input(in, out, di, uo) is det.

	% file__get_line retrieves a line from a file.
	% (Lines are numbered from 0.)
	% Fails if the line is out of bounds.
:- pred file__get_line(file, int, string).
:- mode file__get_line(in, in, out) is semidet.

	% file__get_numlines returns the number of lines
	% in a file.
:- pred file__get_numlines(file, int).
:- mode file__get_numlines(in, out) is det.

	% file__from_list converts a list of lines to a file.
:- pred file__from_list(string, list(string), file).
:- mode file__from_list(in, in, out) is det.

	% file__to_list converts a file into a list of
	% lines.
:- pred file__to_list(file, list(string)).
:- mode file__to_list(in, out) is det.

	% file__get_file_name returns the name of the file.
:- pred file__get_file_name(file, string).
:- mode file__get_file_name(in, out) is det.

	% file__set_file_name sets the name of the file.
:- pred file__set_file_name(file, string, file).
:- mode file__set_file_name(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module array, require, int, bool.

%-----------------------------------------------------------------------------%

:- type file
	--->	file(
			string,			% File name
			array(string)		% Contents
		).

	% Open the stream, read from the stream, then close
	% the stream.
file__read_file(FileName, File) -->
	io__open_input(FileName, Res),
	( { Res = ok(InputStream) },
		file__read_stream(InputStream, Contents),
		io__close_input(InputStream),
		{ File = ok(file(FileName, Contents)) }
	; { Res = error(Error) },
		{ File = error(Error) }
	).

	% Get the input stream, then read from it.
file__read_input(FileName, ok(file(FileName, Contents))) -->
	io__input_stream(InputStream),
	file__read_stream(InputStream, Contents).

	% file__read_stream is the "real" file reader.
:- pred file__read_stream(io__input_stream, array(string),
		io__state, io__state).
:- mode file__read_stream(in, array_uo, di, uo) is det.
file__read_stream(Stream, File) -->
	file__read_stream2(Stream, 0, File).

	% Given a Stream from which LinesIn lines have already been
	% read, fill File[LinesIn] to File[LinesOut-1] with the rest
	% of the lines.  LinesOut is the number of lines in the file.
	% (Note that line numbering starts at zero.)
:- pred file__read_stream2(io__input_stream, int, array(string),
		io__state, io__state).
:- mode file__read_stream2(in, in, array_uo, di, uo) is det.
file__read_stream2(Stream, LineNo, File) -->
	io__read_line(Stream, Res),
	( { Res = eof },
		{ array__init(LineNo, "", File) }
	; { Res = ok(Line) },
		{ string__from_char_list(Line, Line1) },
		{ LineNo1 is LineNo + 1 },
		file__read_stream2(Stream, LineNo1, File1),
		{ array__set(File1, LineNo, Line1, File) }
	; { Res = error(Error) },
		{ io__error_message(Error, Msg) },
		{ error(Msg) }
	).

%-----------------------------------------------------------------------------%

file__get_line(file(_, Contents), LineNo, Line) :-
	array__semidet_lookup(Contents, LineNo, Line).

file__get_numlines(file(_, Contents), NumLines) :-
	array__bounds(Contents, _, NumLines1),
	NumLines is NumLines1 + 1.

%-----------------------------------------------------------------------------%

file__to_list(file(_, Contents), List) :-
	array__to_list(Contents, List).

file__from_list(FileName, List, file(FileName, Contents)) :-
	array__from_list(List, Contents).

%-----------------------------------------------------------------------------%

file__get_file_name(file(FileName, _), FileName).

file__set_file_name(file(_, B), FileName, file(FileName, B)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
