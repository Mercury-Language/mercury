%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module file.
:- interface.
% Main author: bromage

:- import_module io, list, string.

:- type file.

	% file__read_file reads a file from a filename.
:- pred file__read_file(string, file, io__state, io__state).
:- mode file__read_file(in, out, di, uo) is det.

	% file__read_input reads a file from the input
	% stream.
:- pred file__read_input(file, io__state, io__state).
:- mode file__read_input(out, di, uo) is det.

	% file__get_line retrieves a line from a file.
	% Fails if the line is out of bounds.
:- pred file__get_line(file, int, string).
:- mode file__get_line(in, in, out) is semidet.

	% file__get_numlines returns the number of lines
	% in a file.
:- pred file__get_numlines(file, int).
:- mode file__get_numlines(in, out) is det.

	% file__to_list converts a file into a list of
	% lines.
:- pred file__to_list(file, list(string)).
:- mode file__to_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module array, require, int.

%-----------------------------------------------------------------------------%

:- type file == array(string).

	% Open the stream, read from the stream, then close
	% the stream.
file__read_file(FileName, Contents) -->
	io__open_input(FileName, Res),
	( { Res = ok(InputStream) },
	    file__read_stream(InputStream, Contents),
	    io__close_input(InputStream)
	; { Res = error(Error) },
	    { io__error_message(Error, Msg) },
	    { error(Msg) }
	).

	% Get the input stream, then read from it.
file__read_input(Contents) -->
	io__input_stream(InputStream),
	file__read_stream(InputStream, Contents).

	% file__read_stream is the "real" file reader.
:- pred file__read_stream(io__input_stream, file, io__state, io__state).
:- mode file__read_stream(in, out, di, uo) is det.
file__read_stream(Stream, File) -->
	file__read_stream2(Stream, 0, _, File).

:- pred file__read_stream2(io__input_stream, int, int, file,
		io__state, io__state).
:- mode file__read_stream2(in, in, out, out, di, uo) is det.
file__read_stream2(Stream, LinesIn, LinesOut, File) -->
	io__read_line(Stream, Res),
	( { Res = eof },
	    { LinesOut = LinesIn },
	    { array__init(1, LinesOut, "", File) }
	; { Res = ok(Line) },
	    { LinesIn1 is LinesIn+1 },
	    { string__from_char_list(Line, Line1) },
	    file__read_stream2(Stream, LinesIn1, LinesOut, File1),
	    { array__set(File1, LinesIn1, Line1, File) }
	; { Res = error(Error) },
	    { io__error_message(Error, Msg) },
	    { error(Msg) }
	).

%-----------------------------------------------------------------------------%

file__get_line(File, LineNo, Line) :-
	array__semidet_lookup(File, LineNo, Line).

file__get_numlines(File, NumLines) :-
	array__bounds(File, _, NumLines).

file__to_list(File, List) :-
	array__to_list(File, List).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
