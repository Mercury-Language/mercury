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
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.
:- import_module char, int, float, string, std_util, list.

%-----------------------------------------------------------------------------%

% External interface: imported predicate

% :- pred main_predicate(list(string), io__state, io__state).
% :- mode main_predicate(in, di, uo) is det.
%	main_predicate(ArgStrings, IOState0, IOState1).
%		This module provides startup code which calls main_predicate/3.

%-----------------------------------------------------------------------------%

% Exported types

	% The state of the universe.

:- type io__state.

	% Opaque handles for I/O streams.

:- type io__input_stream.

:- type io__output_stream.

	% Various types used for the result from the access predicates

:- type io__result	--->	ok
			;	eof
			;	error.

:- type io__res		--->	ok
			;	error.

:- type io__res(T)	--->	ok(T)
			;	error.


%-----------------------------------------------------------------------------%

% Input predicates.

:- pred io__read_char(character, io__result, io__state, io__state).
:- mode io__read_char(out, out, di, uo) is det.
%		Reads a character from the current input stream.

:- pred io__read_line(list(character), io__result, io__state, io__state).
:- mode io__read_line(out, out, di, uo) is det.
%		Reads a character from the current input stream.

:- pred io__read_char(io__input_stream, character, io__result,
				io__state, io__state).
:- mode io__read_char(in, out, out, di, uo) is det.
%		Reads a character from specified stream.

:- pred io__read_line(io__input_stream, list(character), io__result,
							io__state, io__state).
:- mode io__read_line(in, out, out, di, uo) is det.
%		Reads a character from specified stream.

%-----------------------------------------------------------------------------%

% Output predicates.

:- pred io__write_string(string, io__state, io__state).
:- mode io__write_string(in, di, uo) is det.
%		Writes a string to the current output stream.

:- pred io__write_string(io__output_stream, string, io__state, io__state).
:- mode io__write_string(in, in, di, uo) is det.
%		Writes a string to the specified stream.

:- pred io__write_strings(list(string), io__state, io__state).
:- mode io__write_strings(in, di, uo) is det.
%		Writes a list of strings to the current output stream.

:- pred io__write_strings(io__output_stream, list(string),
				io__state, io__state).
:- mode io__write_strings(in, in, di, uo) is det.
%		Writes a string to the specified stream.

:- pred io__write_char(character, io__state, io__state).
:- mode io__write_char(in, di, uo) is det.
%		Writes a character to the current output stream.

:- pred io__write_char(io__output_stream, character, io__state, io__state).
:- mode io__write_char(in, in, di, uo) is det.
%		Writes a character to the specified stream.

:- pred io__write_int(int, io__state, io__state).
:- mode io__write_int(in, di, uo) is det.
%		Writes an integer to the current output stream.

:- pred io__write_int(io__output_stream, int, io__state, io__state).
:- mode io__write_int(in, in, di, uo) is det.
%		Writes an integer to the specified stream.

:- pred io__write_float(float, io__state, io__state).
:- mode io__write_float(in, di, uo) is det.
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the current output stream.

:- pred io__write_float(io__output_stream, float, io__state, io__state).
:- mode io__write_float(in, in, di, uo) is det.
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the specified stream.

:- pred io__write_anything(_T, io__state, io__state).
:- mode io__write_anything(in, di, uo) is det.
%		Writes it's argument to the current output stream.
%		The argument may be of any type.  This is a hack!

:- pred io__write_anything(io__output_stream, _T, io__state, io__state).
:- mode io__write_anything(in, in, di, uo) is det.
%		Writes it's argument to the specified stream.
%		The argument may be of any type.  This is a hack!

:- pred io__flush_output(io__state, io__state).
:- mode io__flush_output(di, uo) is det.
%	Flush the output buffer of the current output stream.

:- pred io__flush_output(io__output_stream, io__state, io__state).
:- mode io__flush_output(in, di, uo) is det.
%	Flush the output buffer of the specified output stream.

%-----------------------------------------------------------------------------%

% Input stream predicates.

:- pred io__see(string, io__res, io__state, io__state).
:- mode io__see(in, out, di, uo) is det.
%	io__see(File, Result, IO0, IO1).
%		Attempts to open a file for input, and if successful
%		sets the current input stream to the newly opened stream.
%		Result is either 'ok' or 'error'.

:- pred io__seen(io__state, io__state).
:- mode io__seen(di, uo) is det.
%		Closes the current input stream.
%		The current input stream reverts to standard input.

:- pred io__open_input(string, io__res(io__input_stream), io__state, io__state).
:- mode io__open_input(in, out, di, uo) is det.
%	io__open_output(File, Result, IO0, IO1).
%		Attempts to open a file for output.
%		Result is either 'ok(Stream)' or 'error'.

:- pred io__close_input(io__input_stream, io__state, io__state).
:- mode io__close_input(in, di, uo) is det.
%	io__close_input(File, IO0, IO1).
%		Closes an open input stream.

:- pred io__input_stream(io__input_stream, io__state, io__state).
:- mode io__input_stream(out, di, uo) is det.
%		Retrieves the current input stream.
%		Does not modify the IO state.

:- pred io__set_input_stream(io__input_stream, io__input_stream,
				io__state, io__state).
:- mode io__set_input_stream(in, out, di, uo) is det.
%       io__set_input_stream(NewStream, OldStream, IO0, IO1)
%		Changes the current input stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdin_stream(io__input_stream, io__state, io__state).
:- mode io__stdin_stream(out, di, uo) is det.
%		Retrieves the standard input stream.
%		Does not modify the IO state.

:- pred io__input_stream_name(io__input_stream, string, io__state, io__state).
:- mode io__input_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with an stream.
%	For file streams, this is the filename.
%	For stdin this is the string "<standard input>".

:- pred io__get_line_number(int, io__state, io__state).
:- mode io__get_line_number(out, di, uo) is det.

%	Return the line number of the current input stream
%	(as per NU-Prolog lineCount/1).

%-----------------------------------------------------------------------------%

% Output stream predicates.

:- pred io__tell(string, io__res, io__state, io__state).
:- mode io__tell(in, out, di, uo) is det.
%	io__tell(File, Result, IO0, IO1).
%		Attempts to open a file for output, and if successful
%		sets the current output stream to the newly opened stream.
%		As per Prolog tell/1. Result is either 'ok' or 'error'.

:- pred io__told(io__state, io__state).
:- mode io__told(di, uo) is det.
%	io__told(IO0, IO1).
%		Closes the current output stream.
%		The default output stream reverts to standard output.
%		As per Prolog told/0.

:- pred io__open_output(string, io__res(io__output_stream),
				io__state, io__state).
:- mode io__open_output(in, out, di, uo) is det.
%	io__open_output(File, Result, IO0, IO1).
%		Attempts to open a file for output.
%		Result is either 'ok(Stream)' or 'error'.

:- pred io__close_output(io__output_stream, io__state, io__state).
:- mode io__close_output(in, di, uo) is det.
%	io__close_output(File, IO0, IO1).
%		Closes an open output stream.

:- pred io__output_stream(io__output_stream, io__state, io__state).
:- mode io__output_stream(out, di, uo) is det.
%		Retrieves the current output stream.
%		Does not modify the IO state.

:- pred io__set_output_stream(io__output_stream, io__output_stream,
				io__state, io__state).
:- mode io__set_output_stream(in, out, di, uo) is det.
%	io__set_output_stream(NewStream, OldStream, IO0, IO)
%		Changes the current output stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdout_stream(io__output_stream, io__state, io__state).
:- mode io__stdout_stream(out, di, uo) is det.
%		Retrieves the standard output stream.
%		Does not modify the IO state.

:- pred io__stderr_stream(io__output_stream, io__state, io__state).
:- mode io__stderr_stream(out, di, uo) is det.
%		Retrieves the standard error stream.
%		Does not modify the IO state.

:- pred io__output_stream_name(io__output_stream, string, io__state, io__state).
:- mode io__output_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with a stream.
%	For file streams, this is the filename.
%	For stdout this is the string "<standard input>".
%	For stderr this is the string "<standard error>".

%-----------------------------------------------------------------------------%

% Global state predicates.

:- pred io__progname(string, string, io__state, io__state).
:- mode io__progname(in, out, di, uo) is det.
% 	io__progname(DefaultProgname, Progname)
%		Returns the name that the program was invoked with,
%		if available, or DefaultProgname if the name is not
%		available.
%		
%		Does not modify the IO state.

% The io__state includes a `globals' field which is not used by the I/O
% library, but can be used by the application.  The globals field is
% of type `univ' so that the application can store any data it wants there.
% The following predicates can be used to access this global state.

:- pred io__get_globals(univ, io__state, io__state).
:- mode io__get_globals(out, di, uo) is det.
	% Doesn't modify the io__state.

:- pred io__set_globals(univ, io__state, io__state).
:- mode io__set_globals(in, di, uo) is det.

%-----------------------------------------------------------------------------%

% Memory management predicates.

	% Write some memory/time usage statistics to stdout.

:- pred io__report_stats(io__state, io__state).
:- mode io__report_stats(di, uo) is det.

	% Preallocate heap space (to avoid NU-Prolog panic).

:- pred io__preallocate_heap_space(int, io__state, io__state).
:- mode io__preallocate_heap_space(in, di, uo) is det.

:- pred io__gc_call(pred(io__state, io__state), io__state, io__state).
:- mode io__gc_call(
		det_pred(ground_unique, dead, free_unique, ground_unique),
		di, uo) is det.
%	io__gc_call(Goal, IO0, IO1).
%		Execute Goal, passing IO0, and IO1, and
%		collect any garbage created during it's execution.

%-----------------------------------------------------------------------------%

% Miscellaneous predicates

:- pred io__call_system(string, io__res(int), io__state, io__state).
:- mode io__call_system(in, out, di, uo) is det.
%	io__call_system(Command, Result, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Result is either `ok(ExitStatus)', if it was
%		possible to invoke the command, or `error' if not.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map.

/* Most of these predicates are implemented using non-logical NU-Prolog code
   in io.nu.nl. */

:- type io__state	---> 	io__state(io__stream_names, univ, io__state_2).

:- type io__stream_names ==	map(io__stream, string).

:- type io__state_2	---> 	old
			;	current.

:- type io__input_stream ==	io__stream.
:- type io__output_stream ==	io__stream.

:- type io__stream	--->	stream(int, int)
			;	user_input
			;	user_output
			;	user_error.

/*
:- external("NU-Prolog", io__progname/4).
:- external("NU-Prolog", io__write_string/3).
:- external("NU-Prolog", io__read_char/4).
:- external("NU-Prolog", io__read_line/4).
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
:- external("NU-Prolog", io__preallocate_heap_space/3).
*/

io__write_int(Int) -->
	io__output_stream(Stream),
	io__write_int(Stream, Int).

io__write_string(String) -->
	io__output_stream(Stream),
	io__write_string(Stream, String).

io__write_strings(Strings) -->
	io__output_stream(Stream),
	io__write_strings(Stream, Strings).

io__write_strings(_Stream, []) --> [].
io__write_strings(Stream, [S|Ss]) -->
	io__write_string(Stream, S),
	io__write_strings(Stream, Ss).

io__read_char(Char, Result) -->
	io__input_stream(Stream),
	io__read_char(Stream, Char, Result).

io__read_line(String, Result) -->
	io__input_stream(Stream),
	io__read_line(Stream, String, Result).

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

%-----------------------------------------------------------------------------%

	% This is backwards.  `see' and `seen' should be implemented
	% in terms of `open' and `close', not vice versa.  Oh well.
	% I guess it doesn't matter much.

io__open_input(FileName, Result) -->
	io__input_stream(OldStream),
	io__see(FileName, Result0),
	( { Result0 = ok } ->
		io__input_stream(NewStream),
		{ Result = ok(NewStream) }
	;
		{ Result = error }
	),
	io__set_input_stream(OldStream, _).

io__close_input(Stream) -->
	io__set_input_stream(Stream, OldStream),
	io__seen,
	io__set_input_stream(OldStream, _).

io__open_output(FileName, Result) -->
	io__output_stream(OldStream),
	io__tell(FileName, Result0),
	( { Result0 = ok } ->
		io__output_stream(NewStream),
		{ Result = ok(NewStream) }
	;
		{ Result = error }
	),
	io__set_output_stream(OldStream, _).

io__close_output(Stream) -->
	io__set_output_stream(Stream, OldStream),
	io__told,
	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%


io__input_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__output_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

:- pred io__stream_name(io__stream, string, io__state, io__state).
:- mode io__stream_name(in, out, di, uo) is det.

io__stream_name(Stream, Name, IOState, IOState) :-
	IOState = io__state(StreamNames, _, _),
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

io__report_stats -->
	{ report_stats }.

%-----------------------------------------------------------------------------%

io__get_globals(Globals, IOState, IOState) :-
	IOState = io__state(_StreamNames, Globals, _S).

io__set_globals(Globals, io__state(StreamNames, _, S),
		io__state(StreamNames, Globals, S)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
