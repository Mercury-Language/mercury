%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: io.nl.
% Main author: fjh.
%
% This file encapsulates all the file I/O.
% We implement a purely logical I/O system using Prolog's horrible
% non-logical I/O primitives.
%
% This library is still pretty yucko because it's based on
% the old dec-10 Prolog I/O (see, seeing, seen, tell, telling, told)
% instead of the stream-based I/O. 
% TODO: fix this.
%
%-----------------------------------------------------------------------------%

:- module io.
:- import_module char, int, float, string.
:- interface.

% External interface: imported predicate

% :- pred main_predicate(list(string), io__state, io__state).
%	main_predicate(ArgStrings, IOState0, IOState1).
%		This module provides startup code which calls main_predicate/3.

% External interface: exported types

:- export_type io__state.

% External interface: exported predicates

:- pred io__progname(string, io__state, io__state).
:- mode io__progname(output, di, uo).
%		Returns the name that the program was invoked with.
%		Does not modify the IO state.

:- pred io__write_string(string, io__state, io__state).
:- mode io__write_string(input, di, uo).
%		Writes a string to the current output stream.

:- pred io__write_string(io__stream, string, io__state, io__state).
:- mode io__write_string(input, input, di, uo).
%		Writes a string to the specified stream.

:- pred io__write_char(character, io__state, io__state).
:- mode io__write_char(input, di, uo).
%		Writes a character to the current output stream.

:- pred io__write_char(io__stream, character, io__state, io__state).
:- mode io__write_char(input, input, di, uo).
%		Writes a character to the specified stream.

:- pred io__write_int(int, io__state, io__state).
:- mode io__write_int(input, di, uo).
%		Writes an integer to the current output stream.

:- pred io__write_int(io__stream, int, io__state, io__state).
:- mode io__write_int(input, input, di, uo).
%		Writes an integer to the specified stream.

:- pred io__write_float(float, io__state, io__state).
:- mode io__write_float(input, di, uo).
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the current output stream.

:- pred io__write_float(io__stream, float, io__state, io__state).
:- mode io__write_float(input, input, di, uo).
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the specified stream.

:- pred io__write_anything(_T, io__state, io__state).
:- mode io__write_anything(input, di, uo).
%		Writes it's argument to the current output stream.
%		The argument may be of any type.

:- pred io__write_anything(io__stream, _T, io__state, io__state).
:- mode io__write_anything(input, input, di, uo).
%		Writes it's argument to the specified stream.
%		The argument may be of any type.

:- type res ---> ok ; error.
:- pred io__see(string, res, io__state, io__state).
:- mode io__see(input, output, di, uo).
%	io__see(File, Result, IO0, IO1).
%		Attempts to open a file for input, and if successful
%		sets the current input stream to the newly opened stream.
%		Result is either 'ok' or 'error'.

:- pred io__seen(io__state, io__state).
:- mode io__seen(di, uo).
%		Closes the current input stream.
%		The current input stream reverts to standard input.

:- pred io__tell(string, res, io__state, io__state).
:- mode io__tell(input, output, di, uo).
%	io__tell(File, Result, IO0, IO1).
%		Attempts to open a file for output, and if successful
%		sets the current output stream to the newly opened stream.
%		As per Prolog tell/1. Result is either 'ok' or 'error'.

:- pred io__told(io__state, io__state).
:- mode io__told(di, uo).
%	io__told(IO0, IO1).
%		Closes the current output stream.
%		The default output stream reverts to standard output.
%		As per Prolog told/0.

:- pred io__output_stream(io__stream, io__state, io__state).
:- mode io__output_stream(output, di, uo).
%		Retrieves the current output stream.
%		Does not modify the IO state.

:- pred io__set_output_stream(io__stream, io__stream, io__state, io__state).
:- mode io__set_output_stream(input, output, di, uo).
%	io__set_output_stream(NewStream, OldStream, IO0, IO)
%		Changes the current output stream to the stream specified.
%		Returns the previous stream.

:- pred io__input_stream(io__stream, io__state, io__state).
:- mode io__input_stream(output, di, uo).
%		Retrieves the current input stream.
%		Does not modify the IO state.

:- pred io__set_input_stream(io__stream, io__stream, io__state, io__state).
:- mode io__set_input_stream(input, output, di, uo).
%       io__set_input_stream(NewStream, OldStream, IO0, IO1)
%		Changes the current input stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdin_stream(io__stream, io__state, io__state).
:- mode io__stdin_stream(output, di, uo).
%		Retrieves the standard input stream.
%		Does not modify the IO state.

:- pred io__stdout_stream(io__stream, io__state, io__state).
:- mode io__stdout_stream(output, di, uo).
%		Retrieves the standard output stream.
%		Does not modify the IO state.

:- pred io__stderr_stream(io__stream, io__state, io__state).
:- mode io__stderr_stream(output, di, uo).
%		Retrieves the standard error stream.
%		Does not modify the IO state.

:- pred io__get_line_number(int, io__state, io__state).
:- mode io__get_line_number(output, di, uo).
%	Return the line number of the current stream
%	(as per NU-Prolog lineCount/1).

% XXX The type and mode of io__gc_call/3 are a bit tricky.
% :- pred io__gc_call(pred(M, io__state::di, io__state::uo) :: M,
%			io__state::di, io__state::uo).
%	io__gc_call(Goal, IO0, IO1).
%		Execute Goal, passing IO0, and IO1, and
%		collect any garbage created during it's execution.

:- pred io__flush_output(io__state, io__state).
:- mode io__flush_output(di, uo).
%	Flush the output buffer of the current output stream.

:- pred io__flush_output(io__stream, io__state, io__state).
:- mode io__flush_output(input, di, uo).
%	Flush the output buffer of the specified output stream.

:- pred io__stream_name(io__stream, io__filename, io__state, io__state).
:- mode io__stream_name(input, output, di, uo).
%	Retrieves the human-readable name associated with a stream.
%	For file streams, this is the filename.
%	For stdin this is the string "<standard input>" (and similarly
%	for stdout, stderr).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

/* Most of these predicates are implemented using non-logical NU-Prolog code
   in io.nu.nl. */

:- type io__state	---> 	io__state(io__stream_names, io__state_2).

:- type io__stream_names =	map(io__stream, string).

:- type io__state_2	---> 	old
			;	current.

:- type io__stream	--->	stream(int, int)
			;	user_input
			;	user_output
			;	user_error.

/*
:- external("NU-Prolog", io__progname/2).
:- external("NU-Prolog", io__write_string/3).
:- external("NU-Prolog", io__write_char/3).
:- external("NU-Prolog", io__write_int/3).
:- external("NU-Prolog", io__write_float/3).
:- external("NU-Prolog", io__write_anything/3).
:- external("NU-Prolog", io__see/4).
:- external("NU-Prolog", io__seen/2).
:- external("NU-Prolog", io__tell/4).
:- external("NU-Prolog", io__told/2).
:- external("NU-Prolog", io__get_line_number/3).
:- external("NU-Prolog", io__gc_call/3).
:- external("NU-Prolog", io__flush_output/2).
*/

io__write_int(Int) -->
	io__output_stream(Stream),
	io__write_int(Stream, Int).

io__write_string(String) -->
	io__output_stream(Stream),
	io__write_string(Stream, String).

io__write_char(Char) -->
	io__output_stream(Stream),
	io__write_char(Stream, Char).

io__write_float(Float) -->
	io__output_stream(Stream),
	io__write_float(Stream, Float).

io__write_anything(Term) -->
	io__output_stream(Stream),
	io__write_anything(Stream, Term).

io__flush_output -->
	io__output_stream(Stream),
	io__flush_output(Stream).

io__stream_name(Stream, Name, IOState, IOState) :-
	IOState = io__state(StreamNames, _),
	( map__search(StreamNames, Stream, Name1) ->
		Name = Name1
	;
		Name = "<stream name unavailable>"
	).

%-----------------------------------------------------------------------------%

io__stdin_stream(user_input) --> [].

io__stdout_stream(user_output) --> [].

io__stderr_stream(user_error) --> [].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
