%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module file.
:- interface.
:- import_module io, list, string.

:- type file.

:- pred file__read_file(string, file, io__state, io__state).
:- mode file__read_file(in, out, di, uo) is det.

:- pred file__read_input(file, io__state, io__state).
:- mode file__read_input(out, di, uo) is det.

:- pred file__get_line(file, int, string).
:- mode file__get_line(in, in, out) is semidet.

:- pred file__get_numlines(file, int).
:- mode file__get_numlines(in, out) is det.

:- pred file__to_list(file, list(string)).
:- mode file__to_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module array, require, int.

%-----------------------------------------------------------------------------%

:- type file == array(string).

file__read_file(FileName, Contents) -->
	io__open_input(FileName, Res),
	( { Res = ok(InputStream) },
	    file__read_stream(InputStream, Contents),
	    io__close_input(InputStream)
	; { Res = error(Error) },
	    { io__error_message(Error, Msg) },
	    { error(Msg) }
	).

file__read_input(Contents) -->
	io__input_stream(InputStream),
	file__read_stream(InputStream, Contents).

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
	array__bounds(File, Low, High),
	Low =< LineNo, LineNo =< High,
	array__lookup(File, LineNo, Line).

file__get_numlines(File, NumLines) :-
	array__bounds(File, _, NumLines).

file__to_list(File, List) :-
	array__to_list(File, List).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
