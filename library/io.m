%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: io.m.
% Main author: fjh.
% Stability: medium to high.
%
% This file encapsulates all the file I/O.
% We implement a purely logical I/O system using non-logical I/O primitives
% of the underlying system (C or Prolog).
% The logicalness is ensured by passing around a ``state-of-the-world''
% argument using unique modes.  The compiler will check that the state
% of the world argument is properly single-threaded, and will also check
% to ensure that you don't attempt to backtrack over any I/O.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.
:- import_module char, string, std_util, list.

%-----------------------------------------------------------------------------%

% External interface: imported predicate

% :- pred main(io__state, io__state).
% :- mode main(di, uo) is det.
%	main(IOState0, IOState1).
%		This module provides startup code which calls main/2.

%-----------------------------------------------------------------------------%

% Exported types

	% The state of the universe.

:- type io__state.

	% Opaque handles for text I/O streams.

:- type io__input_stream.

:- type io__output_stream.

	% Opaque handles for binary I/O streams.

:- type io__binary_input_stream		==	io__binary_stream.

:- type io__binary_output_stream	==	io__binary_stream.

:- type io__binary_stream.

	% Various types used for the result from the access predicates

:- type io__res		--->	ok
			;	error(io__error).

:- type io__res(T)	--->	ok(T)
			;	error(io__error).

:- type io__result	--->	ok
			;	eof
			;	error(io__error).

:- type io__result(T)	--->	ok(T)
			;	eof
			;	error(io__error).

:- type io__read_result(T)	--->	ok(T)
				;	eof
				;	error(string, int).
					% error message, line number

:- type io__error.	% Use io__error_message to decode it.

	% Poly-type is used for io__write_many and io__format,
	% which do printf-like formatting.

:- type io__poly_type == string__poly_type.
%			--->
%		c(char)
%	;	s(string)
%	;	i(int)
%	;	f(float).
%

	% io__whence denotes the base for a seek operation.
	% 	set	- seek relative to the start of the file
	%	cur	- seek relative to the current position in the file
	%	end	- seek relative to the end of the file.

:- type io__whence
	--->	set
	;	cur
	;	end
	.

%-----------------------------------------------------------------------------%

% Text input predicates.

:- pred io__read_char(io__result(char), io__state, io__state).
:- mode io__read_char(out, di, uo) is det.
%		Reads a character from the current input stream.

:- pred io__read_word(io__result(list(char)), io__state, io__state).
:- mode io__read_word(out, di, uo) is det.
%		Reads a whitespace delimited word from the current input stream.

:- pred io__read_line(io__result(list(char)), io__state, io__state).
:- mode io__read_line(out, di, uo) is det.
%		Reads a line from the current input stream, returns the
%		the result as a list of chars.

:- pred io__read_line_as_string(io__result(string), io__state, io__state).
:- mode io__read_line_as_string(out, di, uo) is det.
%		Reads a line from the current input stream, returns the
%		result as a string.

:- pred io__read_file(io__result(list(char)), io__state, io__state).
:- mode io__read_file(out, di, uo) is det.
%		Reads all the characters from the current input stream until
%		eof or error.

:- pred io__read_file_as_string(io__res, string, io__state, io__state).
:- mode io__read_file_as_string(out, out, di, uo) is det.
%		Reads all the characters from the current input stream until
%		eof or error.  Returns the result as a string rather than
%		as a list of char.

:- pred io__putback_char(char, io__state, io__state).
:- mode io__putback_char(in, di, uo) is det.
%		Un-reads a character from the current input stream.
%		You can put back as many characters as you like.
%		You can even put back something that you didn't actually read.
%		Note: `io__putback_char' uses the C library function ungetc().
%		On some systems only one character of pushback is guaranteed.
%		`io__putback_char' will throw an io__error exception
%		if ungetc() fails.

:- pred io__read_char(io__input_stream, io__result(char),
				io__state, io__state).
:- mode io__read_char(in, out, di, uo) is det.
%		Reads a character from specified stream.

:- pred io__read_word(io__input_stream, io__result(list(char)),
							io__state, io__state).
:- mode io__read_word(in, out, di, uo) is det.
%		Reads a whitespace delimited word from specified stream.

:- pred io__read_line(io__input_stream, io__result(list(char)),
							io__state, io__state).
:- mode io__read_line(in, out, di, uo) is det.
%		Reads a line from specified stream, returning the result
%		as a list of chars.

:- pred io__read_line_as_string(io__input_stream, io__result(string),
		io__state, io__state).
:- mode io__read_line_as_string(in, out, di, uo) is det.
%		Reads a line from specified stream, returning the
%		result as a string.

:- pred io__read_file(io__input_stream, io__result(list(char)),
							io__state, io__state).
:- mode io__read_file(in, out, di, uo) is det.
%		Reads all the characters from the given input stream until
%		eof or error.

:- pred io__read_file_as_string(io__input_stream, io__res, string,
							io__state, io__state).
:- mode io__read_file_as_string(in, out, out, di, uo) is det.
%		Reads all the characters from the given input stream until
%		eof or error.  Returns the result as a string rather than
%		as a list of char.

:- pred io__putback_char(io__input_stream, char, io__state, io__state).
:- mode io__putback_char(in, in, di, uo) is det.
%		Un-reads a character from specified stream.
%		You can put back as many characters as you like.
%		You can even put back something that you didn't actually read.
%		Note: `io__putback_char' uses the C library function ungetc().
%		On some systems only one character of pushback is guaranteed.
%		`io__putback_char' will throw an io__error exception
%		if ungetc() fails.

:- pred io__read(io__read_result(T), io__state, io__state).
:- mode io__read(out, di, uo) is det.
:- pred io__read(io__input_stream, io__read_result(T), io__state, io__state).
:- mode io__read(in, out, di, uo) is det.
%		Reads a ground term of any type, written using standard
%		Mercury syntax, from the current or specified input stream.
%		The type of the term read is determined by the context
%		in which `io__read' is used.
%		If there are no more non-whitespace characters before the
%		end of file, then `io__read' returns `eof'.
%		If it can read in a syntactically correct ground term
%		of the correct type, then it returns `ok(Term)'.
%		If characters on the input stream (up to the next `.' that
%		is followed by whitespace) do not form a syntactically
%		correct term, or if the term read is not a ground term,
%		if the term is not a valid term of the appropriate type,
%		or if encounters an I/O error, then it returns
%		`error(Message, LineNumber)'.

% The type `posn' represents a position within a string.
:- type posn
	--->	posn(int, int, int).
		% line number, offset of start of line, current offset
		% (the first two are used only for the purposes of
		% computing term_contexts, for use e.g. in error messages).
		% Offsets start at zero.

:- pred io__read_from_string(string, string, int, io__read_result(T),
				posn, posn).
:- mode io__read_from_string(in, in, in, out, in, out) is det.
% mode io__read_from_string(FileName, String, MaxPos, Result, Posn0, Posn):
%		Same as io__read/4 except that it reads from
%		a string rather than from a stream.
%		FileName is the name of the source (for use in error messages).
%		String is the string to be parsed.
%		Posn0 is the position to start parsing from.
%		Posn is the position one past where the term read in ends.
%		MaxPos is the offset in the string which should be
%		considered the end-of-stream -- this is the upper bound
%		for Posn.  (In the usual case, MaxPos is just the length
%		of the String.)
%		WARNING: if MaxPos > length of String then the behaviour
%		is UNDEFINED.

:- pred io__ignore_whitespace(io__result, io__state, io__state).
:- mode io__ignore_whitespace(out, di, uo) is det.
%		Discards all the whitespace from the current stream.

:- pred io__ignore_whitespace(io__input_stream, io__result,
				io__state, io__state).
:- mode io__ignore_whitespace(in, out, di, uo) is det.
%		Discards all the whitespace from the specified stream.



%-----------------------------------------------------------------------------%

% Text output predicates.
% These will all throw an io__error exception if an I/O error occurs.

:- pred io__print(T, io__state, io__state).
:- mode io__print(in, di, uo) is det.
:- pred io__print(io__output_stream, T, io__state, io__state).
:- mode io__print(in, in, di, uo) is det.
%		io__print/3 writes its argument to the current output stream.
%		io__print/4 writes its argument to the specified output
%		stream.  In either case, the argument may be of any type.
%		The argument is written in a format that is intended to
%		be human-readable. 
%
%		If the argument is just a single string or character, it
%		will be printed out exactly as is (unquoted).
%		If the argument is of type univ, then it will print out
%		the value stored in the univ, but not the type.
%		For higher-order types, or for types defined using the
%		foreign language interface (pragma c_code), the text output
%		will only describe the type that is being printed, not the
%		value.

:- pred io__write(T, io__state, io__state).
:- mode io__write(in, di, uo) is det.
:- pred io__write(io__output_stream, T, io__state, io__state).
:- mode io__write(in, in, di, uo) is det.
%		io__write/3 writes its argument to the current output stream.
%		io__write/4 writes its argument to the specified output stream.
%		The argument may be of any type.
%		The argument is written in a format that is intended to
%		be valid Mercury syntax whenever possible.
%
%		Strings and characters are always printed out in quotes,
%		using backslash escapes if necessary.
%		For higher-order types, or for types defined using the
%		foreign language interface (pragma c_code), the text output
%		will only describe the type that is being printed, not the
%		value, and the result may not be parsable by io__read.
%		But in all other cases the format used is standard Mercury
%		syntax, and if you do append a period and newline (".\n"),
%		then the results can be read in again using `io__read'.

:- pred io__nl(io__state, io__state).
:- mode io__nl(di, uo) is det.
%		Writes a newline character to the current output stream.

:- pred io__nl(io__output_stream, io__state, io__state).
:- mode io__nl(in, di, uo) is det.
%		Writes a newline character to the specified output stream.

:- pred io__write_string(string, io__state, io__state).
:- mode io__write_string(in, di, uo) is det.
%		Writes a string to the current output stream.

:- pred io__write_string(io__output_stream, string, io__state, io__state).
:- mode io__write_string(in, in, di, uo) is det.
%		Writes a string to the specified output stream.

:- pred io__write_strings(list(string), io__state, io__state).
:- mode io__write_strings(in, di, uo) is det.
%		Writes a list of strings to the current output stream.

:- pred io__write_strings(io__output_stream, list(string),
				io__state, io__state).
:- mode io__write_strings(in, in, di, uo) is det.
%		Writes a list of strings to the specified output stream.

:- pred io__write_char(char, io__state, io__state).
:- mode io__write_char(in, di, uo) is det.
%		Writes a character to the current output stream.

:- pred io__write_char(io__output_stream, char, io__state, io__state).
:- mode io__write_char(in, in, di, uo) is det.
%		Writes a character to the specified output stream.

:- pred io__write_int(int, io__state, io__state).
:- mode io__write_int(in, di, uo) is det.
%		Writes an integer to the current output stream.

:- pred io__write_int(io__output_stream, int, io__state, io__state).
:- mode io__write_int(in, in, di, uo) is det.
%		Writes an integer to the specified output stream.

:- pred io__write_float(float, io__state, io__state).
:- mode io__write_float(in, di, uo) is det.
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the current output stream.

:- pred io__write_float(io__output_stream, float, io__state, io__state).
:- mode io__write_float(in, in, di, uo) is det.
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to the specified output stream.

:- pred io__format(string, list(io__poly_type), io__state, io__state).
:- mode io__format(in, in, di, uo) is det.
%	io__format(FormatString, Arguments, IO0, IO).
%		Formats the specified arguments according to
%		the format string, using string__format, and
%		then writes the result to the current output stream.
%		(See the documentation of string__format for details.)

:- pred io__format(io__output_stream, string, list(io__poly_type),
		io__state, io__state).
:- mode io__format(in, in, in, di, uo) is det.
%	io__format(Stream, FormatString, Arguments, IO0, IO).
%		Formats the specified argument list according to
%		the format string, using string__format, and
%		then writes the result to the specified output stream.
%		(See the documentation of string__format for details.)

:- pred io__write_many(list(io__poly_type), io__state, io__state).
:- mode io__write_many(in, di, uo) is det.
%	io__write_many(Arguments, IO0, IO).
%		Writes the specified arguments to the current output stream.

:- pred io__write_many(io__output_stream, list(io__poly_type),
			io__state, io__state).
:- mode io__write_many(in, in, di, uo) is det.
%	io__write_many(Stream, Arguments, IO0, IO).
%		Writes the specified arguments to the specified output stream.

:- pred io__write_list(list(T), string, pred(T, io__state, io__state),
	io__state, io__state).
:- mode io__write_list(in, in, pred(in, di, uo) is det, di, uo) is det.
	% io__write_list(List, Separator, OutputPred, IO0, IO)
	% applies OutputPred to each element of List, printing Separator
	% between each element. Outputs to the current output stream.

:- pred io__write_list(io__output_stream, list(T), string, 
	pred(T, io__state, io__state), io__state, io__state).
:- mode io__write_list(in, in, in, pred(in, di, uo) is det, di, uo) is det.
	% io__write_list(Stream, List, Separator, OutputPred, IO0, IO)
	% applies OutputPred to each element of List, printing Separator
	% between each element. Outputs to Stream.

:- pred io__flush_output(io__state, io__state).
:- mode io__flush_output(di, uo) is det.
%	Flush the output buffer of the current output stream.

:- pred io__flush_output(io__output_stream, io__state, io__state).
:- mode io__flush_output(in, di, uo) is det.
%	Flush the output buffer of the specified output stream.

%-----------------------------------------------------------------------------%

% Input text stream predicates.

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
%		This will all throw an io__error exception
%		if an I/O error occurs.

:- pred io__open_input(string, io__res(io__input_stream),
			io__state, io__state).
:- mode io__open_input(in, out, di, uo) is det.
%	io__open_input(File, Result, IO0, IO1).
%		Attempts to open a file for input.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_input(io__input_stream, io__state, io__state).
:- mode io__close_input(in, di, uo) is det.
%	io__close_input(File, IO0, IO1).
%		Closes an open input stream.
%		This will all throw an io__error exception
%		if an I/O error occurs.

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

:- pred io__input_stream_name(string, io__state, io__state).
:- mode io__input_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current input
%	stream.
%	For file streams, this is the filename.
%	For stdin this is the string "<standard input>".

:- pred io__input_stream_name(io__input_stream, string, io__state, io__state).
:- mode io__input_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified input
%	stream.
%	For file streams, this is the filename.
%	For stdin this is the string "<standard input>".

:- pred io__get_line_number(int, io__state, io__state).
:- mode io__get_line_number(out, di, uo) is det.
%	Return the line number of the current input stream.
%	Lines are normally numbered starting at 1
%	(but this can be overridden by calling io__set_line_number).

:- pred io__get_line_number(io__input_stream, int, io__state, io__state).
:- mode io__get_line_number(in, out, di, uo) is det.
%	Return the line number of the specified input stream.
%	Lines are normally numbered starting at 1
%	(but this can be overridden by calling io__set_line_number).

:- pred io__set_line_number(int, io__state, io__state).
:- mode io__set_line_number(in, di, uo) is det.
%	Set the line number of the current input stream.

:- pred io__set_line_number(io__input_stream, int, io__state, io__state).
:- mode io__set_line_number(in, in, di, uo) is det.
%	Set the line number of the specified input stream.

%-----------------------------------------------------------------------------%

% Output text stream predicates.

:- pred io__tell(string, io__res, io__state, io__state).
:- mode io__tell(in, out, di, uo) is det.
%	io__tell(File, Result, IO0, IO1).
%		Attempts to open a file for output, and if successful
%		sets the current output stream to the newly opened stream.
%		As per Prolog tell/1. Result is either 'ok' or 'error(ErrCode)'.

:- pred io__told(io__state, io__state).
:- mode io__told(di, uo) is det.
%	io__told(IO0, IO1).
%		Closes the current output stream.
%		The default output stream reverts to standard output.
%		As per Prolog told/0.
%		This will all throw an io__error exception
%		if an I/O error occurs.

:- pred io__open_output(string, io__res(io__output_stream),
				io__state, io__state).
:- mode io__open_output(in, out, di, uo) is det.
%	io__open_output(File, Result, IO0, IO1).
%		Attempts to open a file for output.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__open_append(string, io__res(io__output_stream),
				io__state, io__state).
:- mode io__open_append(in, out, di, uo) is det.
%	io__open_append(File, Result, IO0, IO1).
%		Attempts to open a file for appending.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_output(io__output_stream, io__state, io__state).
:- mode io__close_output(in, di, uo) is det.
%	io__close_output(File, IO0, IO1).
%		Closes an open output stream.
%		This will all throw an io__error exception
%		if an I/O error occurs.

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

:- pred io__output_stream_name(string, io__state, io__state).
:- mode io__output_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current
%	output stream.
%	For file streams, this is the filename.
%	For stdout this is the string "<standard output>".
%	For stderr this is the string "<standard error>".

:- pred io__output_stream_name(io__output_stream, string,
				io__state, io__state).
:- mode io__output_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified stream.
%	For file streams, this is the filename.
%	For stdout this is the string "<standard output>".
%	For stderr this is the string "<standard error>".

:- pred io__get_output_line_number(int, io__state, io__state).
:- mode io__get_output_line_number(out, di, uo) is det.
%	Return the line number of the current output stream.
%	Lines are normally numbered starting at 1
%	(but this can be overridden by calling io__set_output_line_number).

:- pred io__get_output_line_number(io__output_stream, int,
				io__state, io__state).
:- mode io__get_output_line_number(in, out, di, uo) is det.
%	Return the line number of the specified output stream.
%	Lines are normally numbered starting at 1
%	(but this can be overridden by calling io__set_output_line_number).

:- pred io__set_output_line_number(int, io__state, io__state).
:- mode io__set_output_line_number(in, di, uo) is det.
%	Set the line number of the current output stream.

:- pred io__set_output_line_number(io__output_stream, int,
				io__state, io__state).
:- mode io__set_output_line_number(in, in, di, uo) is det.
%	Set the line number of the specified output stream.


%-----------------------------------------------------------------------------%

% Binary input predicates.

:- pred io__read_binary(io__result(T), io__state, io__state).
:- mode io__read_binary(out, di, uo) is det.
%		Reads a binary representation of a term of type T
%		from the current binary input stream.

:- pred io__read_binary(io__binary_input_stream, io__result(T),
		io__state, io__state).
:- mode io__read_binary(in, out, di, uo) is det.
%		Reads a binary representation of a term of type T
%		from the specified binary input stream.

%		Note: if you attempt to read a binary representation written
%		by a different program, or a different version of the same
%		program, then the results are not guaranteed to be meaningful.
%		Another caveat is that higher-order types cannot be read. 
%		(If you try, you will get a runtime error.)

:- pred io__read_byte(io__result(int), io__state, io__state).
:- mode io__read_byte(out, di, uo) is det.
%		Reads a single 8-bit byte from the current binary input
%		stream.

:- pred io__read_byte(io__binary_input_stream, io__result(int),
				io__state, io__state).
:- mode io__read_byte(in, out, di, uo) is det.
%		Reads a single 8-bit byte from the specified binary input
%		stream.

:- pred io__read_binary_file(io__result(list(int)), io__state, io__state).
:- mode io__read_binary_file(out, di, uo) is det.
%		Reads all the bytes from the current binary input stream
%		until eof or error.

:- pred io__read_binary_file(io__input_stream, io__result(list(int)),
						io__state, io__state).
:- mode io__read_binary_file(in, out, di, uo) is det.
%		Reads all the bytes from the given binary input stream until
%		eof or error.

:- pred io__putback_byte(int, io__state, io__state).
:- mode io__putback_byte(in, di, uo) is det.
%		Un-reads a byte from the current binary input stream.
%		You can put back as many bytes as you like.
%		You can even put back something that you didn't actually read.
%		The byte is taken from the bottom 8 bits of an integer.

:- pred io__putback_byte(io__binary_input_stream, int, io__state, io__state).
:- mode io__putback_byte(in, in, di, uo) is det.
%		Un-reads a byte from specified binary input stream.
%		You can put back as many bytes as you like.
%		You can even put back something that you didn't actually read.
%		The byte is returned in the bottom 8 bits of an integer.

%-----------------------------------------------------------------------------%

% Binary output predicates.
% These will all throw an io__error exception if an I/O error occurs.

% XXX what about wide characters?

:- pred io__write_binary(T, io__state, io__state).
:- mode io__write_binary(in, di, uo) is det.
%		Writes a binary representation of a term to the current
%		binary output stream, in a format suitable for reading
%		in again with io__read_binary.

:- pred io__write_binary(io__binary_output_stream, T, io__state, io__state).
:- mode io__write_binary(in, in, di, uo) is det.
%		Writes a binary representation of a term to the specified
%		binary output stream, in a format suitable for reading
%		in again with io__read_binary.

:- pred io__write_byte(int, io__state, io__state).
:- mode io__write_byte(in, di, uo) is det.
%		Writes a single byte to the current binary output stream.
%		The byte is taken from the bottom 8 bits of an int.

:- pred io__write_byte(io__binary_output_stream, int, io__state, io__state).
:- mode io__write_byte(in, in, di, uo) is det.
%		Writes a single byte to the specified binary output stream.
%		The byte is taken from the bottom 8 bits of an int.

:- pred io__write_bytes(string, io__state, io__state).
:- mode io__write_bytes(in, di, uo) is det.
%		Writes several bytes to the current binary output stream.
%		The bytes are taken from a string.

:- pred io__write_bytes(io__binary_output_stream, string,
				io__state, io__state).
:- mode io__write_bytes(in, in, di, uo) is det.
%		Writes several bytes to the specified binary output stream.
%		The bytes are taken from a string.

:- pred io__flush_binary_output(io__state, io__state).
:- mode io__flush_binary_output(di, uo) is det.
%	Flush the output buffer of the current binary output stream.

:- pred io__flush_binary_output(io__binary_output_stream,
				io__state, io__state).
:- mode io__flush_binary_output(in, di, uo) is det.
%	Flush the output buffer of the specified binary output stream.

:- pred io__seek_binary(io__binary_stream, io__whence, int,
				io__state, io__state).
:- mode io__seek_binary(in, in, in, di, uo) is det.
%	Seek to an offset relative to Whence (documented above)
%	on a specified binary stream. Attempting to seek on a
%	pipe or tty results in implementation dependent behaviour.

:- pred io__binary_stream_offset(io__binary_stream, int,
				io__state, io__state).
:- mode io__binary_stream_offset(in, out, di, uo) is det.
%	Returns the offset (in bytes) into the specified binary stream.

%-----------------------------------------------------------------------------%

% Binary input stream predicates.

:- pred io__see_binary(string, io__res, io__state, io__state).
:- mode io__see_binary(in, out, di, uo) is det.
%	io__see_binary(File, Result, IO0, IO1).
%		Attempts to open a file for binary input, and if successful
%		sets the current binary input stream to the newly opened stream.
%		Result is either 'ok' or 'error'.

:- pred io__seen_binary(io__state, io__state).
:- mode io__seen_binary(di, uo) is det.
%		Closes the current input stream.
%		The current input stream reverts to standard input.
%		This will all throw an io__error exception
%		if an I/O error occurs.

:- pred io__open_binary_input(string, io__res(io__binary_input_stream),
			io__state, io__state).
:- mode io__open_binary_input(in, out, di, uo) is det.
%	io__open_binary_input(File, Result, IO0, IO1).
%		Attempts to open a binary file for input.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_binary_input(io__binary_input_stream, io__state, io__state).
:- mode io__close_binary_input(in, di, uo) is det.
%	io__close_binary_input(File, IO0, IO1).
%		Closes an open binary input stream.
%		This will all throw an io__error exception
%		if an I/O error occurs.

:- pred io__binary_input_stream(io__binary_input_stream,
			io__state, io__state).
:- mode io__binary_input_stream(out, di, uo) is det.
%		Retrieves the current binary input stream.
%		Does not modify the IO state.

:- pred io__set_binary_input_stream(io__binary_input_stream,
			io__binary_input_stream, io__state, io__state).
:- mode io__set_binary_input_stream(in, out, di, uo) is det.
%       io__set_binary_input_stream(NewStream, OldStream, IO0, IO1)
%		Changes the current input stream to the stream specified.
%		Returns the previous stream.

:- pred io__stdin_binary_stream(io__binary_input_stream,
			io__state, io__state).
:- mode io__stdin_binary_stream(out, di, uo) is det.
%		Retrieves the standard binary input stream.
%		Does not modify the IO state.

:- pred io__binary_input_stream_name(string, io__state, io__state).
:- mode io__binary_input_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current binary
%	input stream.
%	For file streams, this is the filename.

:- pred io__binary_input_stream_name(io__binary_input_stream, string,
		io__state, io__state).
:- mode io__binary_input_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified binary
%	input stream.
%	For file streams, this is the filename.

%-----------------------------------------------------------------------------%

% Binary output stream predicates.

:- pred io__tell_binary(string, io__res, io__state, io__state).
:- mode io__tell_binary(in, out, di, uo) is det.
%	io__tell_binary(File, Result, IO0, IO1).
%		Attempts to open a file for binary output, and if successful
%		sets the current binary output stream to the newly opened
%		stream. As per Prolog tell/1. Result is either 'ok' or
%		'error(ErrCode)'.

:- pred io__told_binary(io__state, io__state).
:- mode io__told_binary(di, uo) is det.
%	io__told_binary(IO0, IO1).
%		Closes the current binary output stream.
%		The default binary output stream reverts to standard output.
%		As per Prolog told/0.
%		This will all throw an io__error exception
%		if an I/O error occurs.

:- pred io__open_binary_output(string, io__res(io__binary_output_stream),
				io__state, io__state).
:- mode io__open_binary_output(in, out, di, uo) is det.
%	io__open_binary_output(File, Result, IO0, IO1).
%		Attempts to open a file for binary output.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__open_binary_append(string, io__res(io__binary_output_stream),
				io__state, io__state).
:- mode io__open_binary_append(in, out, di, uo) is det.
%	io__open_binary_append(File, Result, IO0, IO1).
%		Attempts to open a file for binary appending.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

:- pred io__close_binary_output(io__binary_output_stream,
				io__state, io__state).
:- mode io__close_binary_output(in, di, uo) is det.
%	io__close_binary_output(File, IO0, IO1).
%		Closes an open binary output stream.
%		This will all throw an io__error exception
%		if an I/O error occurs.

:- pred io__binary_output_stream(io__binary_output_stream,
				io__state, io__state).
:- mode io__binary_output_stream(out, di, uo) is det.
%		Retrieves the current binary output stream.
%		Does not modify the IO state.

:- pred io__stdout_binary_stream(io__binary_output_stream,
				io__state, io__state).
:- mode io__stdout_binary_stream(out, di, uo) is det.
%		Retrieves the standard binary output stream.
%		Does not modify the IO state.

:- pred io__set_binary_output_stream(io__binary_output_stream,
			io__binary_output_stream, io__state, io__state).
:- mode io__set_binary_output_stream(in, out, di, uo) is det.
%	io__set_binary_output_stream(NewStream, OldStream, IO0, IO)
%		Changes the current binary output stream to the stream
%		specified. Returns the previous stream.

:- pred io__binary_output_stream_name(string, io__state, io__state).
:- mode io__binary_output_stream_name(out, di, uo) is det.
%	Retrieves the human-readable name associated with the current
%	binary output stream.
%	For file streams, this is the filename.

:- pred io__binary_output_stream_name(io__binary_output_stream, string,
			io__state, io__state).
:- mode io__binary_output_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified 
%	output stream.
%	For file streams, this is the filename.

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

:- pred io__progname_base(string, string, io__state, io__state).
:- mode io__progname_base(in, out, di, uo) is det.
% 	io__progname_base(DefaultProgname, Progname)
%		Like `io__progname', except that it strips off any path name
%		preceding the program name.  Useful for error messages.

:- pred io__command_line_arguments(list(string), io__state, io__state).
:- mode io__command_line_arguments(out, di, uo) is det.
% 	io__command_line_arguments(Args)
%		Returns the arguments that the program was invoked with,
%		if available, otherwise an empty list.
%		
%		Does not modify the IO state.

% The io__state contains an integer used to record the program's exit
% status.  When the program finishes, it will return this exit status to
% the operating system.  The following predicates can be used to get and
% set the exit status.

:- pred io__get_exit_status(int, io__state, io__state).
:- mode io__get_exit_status(out, di, uo) is det.

:- pred io__set_exit_status(int, io__state, io__state).
:- mode io__set_exit_status(in, di, uo) is det.

% The io__state includes a `globals' field which is not used by the I/O
% library, but can be used by the application.  The globals field is
% of type `univ' so that the application can store any data it wants there.
% The following predicates can be used to access this global state.

:- pred io__get_globals(univ, io__state, io__state).
:- mode io__get_globals(uo, di, uo) is det.
	% Doesn't modify the io__state.

:- pred io__set_globals(univ, io__state, io__state).
:- mode io__set_globals(di, di, uo) is det.

% The following predicates provide an interface to the environment list.
% Do not attempt to put spaces or '=' signs in the names of environment
% variables, or bad things may result!

:- pred io__get_environment_var(string, maybe(string), io__state, io__state).
:- mode io__get_environment_var(in, out, di, uo) is det.
	% First argument is the name of the environment variable.
	% Returns yes(Value) if the variable was set (Value will
	% be set to the value of the variable) and no if the
	% variable was not set.

:- pred io__set_environment_var(string, string, io__state, io__state).
:- mode io__set_environment_var(in, in, di, uo) is det.
	% First argument is the name of the environment variable,
	% second argument is the value to be assigned to that
	% variable.  Will throw an exception if the system runs
	% out of environment space.

%-----------------------------------------------------------------------------%

% File handling predicates

:- pragma obsolete(io__tmpnam/3). % use io__make_temp/3 instead
:- pred io__tmpnam(string, io__state, io__state).
:- mode io__tmpnam(out, di, uo) is det.
	% io__tmpnam(Name, IO0, IO) binds `Name' to a temporary
	% file name which is different to the name of any existing file.
	% It will reside in /tmp if the TMPDIR environment variable
	% is not set, or in the directory specified by TMPDIR if it
	% is set.
	% Use of this predicate is deprecated, because it may
	% result in race conditions.  Use io__make_temp/3 instead.

:- pragma obsolete(io__tmpnam/5). % use io__make_temp/5 instead
:- pred io__tmpnam(string, string, string, io__state, io__state).
:- mode io__tmpnam(in, in, out, di, uo) is det.
	% io__tmpnam(Dir, Prefix, Name, IO0, IO) binds `Name' to a
	% temporary file name which is different to the name of any
	% existing file. It will reside in the directory specified by
	% `Dir' and have a prefix using up to the first 5 characters
	% of `Prefix'.
	% Use of this predicate is deprecated, because it may
	% result in race conditions.  Use io__make_temp/5 instead.

:- pred io__make_temp(string, io__state, io__state).
:- mode io__make_temp(out, di, uo) is det.
	% io__make_temp(Name, IO0, IO) creates an empty file
	% whose name which is different to the name of any existing file.
	% Name is bound to the name of the file.
	% The file will reside in /tmp if the TMPDIR environment variable
	% is not set, or in the directory specified by TMPDIR if it
	% is set.
	% It is the responsibility of the program to delete the file
	% when it is no longer needed.

:- pred io__make_temp(string, string, string, io__state, io__state).
:- mode io__make_temp(in, in, out, di, uo) is det.
	% io__mktemp(Dir, Prefix, Name, IO0, IO) creates an empty
	% file whose name is different to the name of any existing file.
	% The file will reside in the directory specified by `Dir' and will
	% have a prefix using up to the first 5 characters of `Prefix'.
	% Name is bound to the name of the file.
	% It is the responsibility of the program to delete the file
	% when it is no longer needed.

:- pred io__remove_file(string, io__res, io__state, io__state).
:- mode io__remove_file(in, out, di, uo) is det.
	% io__remove_file(FileName, Result, IO0, IO) attempts to remove the
	% file `FileName', binding Result to ok/0 if it succeeds, or
	% error/1 if it fails.
	% If `FileName' names a file that is currently open,
	% the behaviour is implementation-dependent.

:- pred io__rename_file(string, string, io__res, io__state, io__state).
:- mode io__rename_file(in, in, out, di, uo) is det.
	% io__rename_file(OldFileName, NewFileName, Result, IO0, IO)
	% attempts to rename the file `OldFileName' as `NewFileName',
	% binding Result to ok/0 if it succeeds, or error/1 if it fails.
	% If `OldFileName' names a file that is currently open,
	% the behaviour is implementation-dependent.
	% If `NewFileName' names a file that already exists
	% the behaviour is also implementation-dependent;
	% on some systems, the file previously named `NewFileName' will be
	% deleted and replaced with the file previously named `OldFileName'.

%-----------------------------------------------------------------------------%

% Memory management predicates.

	% Write memory/time usage statistics to stdout.

:- pred io__report_stats(io__state, io__state).
:- mode io__report_stats(di, uo) is det.

	% Write complete memory usage statistics to stdout,
	% including information about all procedures and types.
	% (You need to compile with memory profiling enabled.)

:- pred io__report_full_memory_stats(io__state, io__state).
:- mode io__report_full_memory_stats(di, uo) is det.

/*** no longer supported, sorry
:- pred io__gc_call(pred(io__state, io__state), io__state, io__state).
:- mode io__gc_call(pred(di, uo) is det, di, uo) is det.
%	io__gc_call(Goal, IO0, IO1).
%		Execute Goal, passing IO0, and IO1, and
%		collect any garbage created during it's execution.
***/

%-----------------------------------------------------------------------------%

% Miscellaneous predicates

:- pred io__call_system(string, io__res(int), io__state, io__state).
:- mode io__call_system(in, out, di, uo) is det.
%	io__call_system(Command, Result, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Result is either `ok(ExitStatus)', if it was
%		possible to invoke the command, or `error(ErrorCode)' if not.
%		The ExitStatus will be 0 if the command completed
%		successfully or the return value of the system call.  If a
%		signal kills the system call, then Result will be an error
%		indicating which signal occured.

:- pred io__error_message(io__error, string).
:- mode io__error_message(in, out) is det.
%	io__error_message(ErrorCode, ErrorMessage).
%		Look up the error message corresponding to a particular error
%		code.

%-----------------------------------------------------------------------------%
:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%
:- interface.

% For backwards compatibility:

:- pragma obsolete(io__read_anything/3).
:- pred io__read_anything(io__read_result(T), io__state, io__state).
:- mode io__read_anything(out, di, uo) is det.
%		Same as io__read/3.

:- pragma obsolete(io__read_anything/4).
:- pred io__read_anything(io__output_stream, io__read_result(T),
			io__state, io__state).
:- mode io__read_anything(in, out, di, uo) is det.
%		Same as io__read/4.

:- pragma obsolete(io__write_anything/3).
:- pred io__write_anything(T, io__state, io__state).
:- mode io__write_anything(in, di, uo) is det.
%		Same as io__write/3.

:- pragma obsolete(io__write_anything/4).
:- pred io__write_anything(io__output_stream, T, io__state, io__state).
:- mode io__write_anything(in, in, di, uo) is det.
%		Same as io__write/4.

% For use by term_io.m:

:- import_module ops.

:- pred io__get_op_table(ops__table, io__state, io__state).
:- mode io__get_op_table(out, di, uo) is det.

:- pred io__set_op_table(ops__table, io__state, io__state).
:- mode io__set_op_table(di, di, uo) is det.

% For use by browser/browse.m:

:- pred io__write_univ(univ, io__state, io__state).
:- mode io__write_univ(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, dir, term, term_io, varset, require, benchmarking, array.
:- import_module int, parser, exception.

:- type io__state ---> io__state(c_pointer).
	% Values of type `io__state' are never really used:
	% instead we store data in global variables.
	% The reason this is not defined simply as `io__state == c_pointer'
	% is so that `type_name' produces more informative results
	% for cases such as `type_name(main)'.

:- pragma c_header_code("
	extern Word ML_io_stream_names;
	extern Word ML_io_user_globals;
	#if 0
	  extern Word ML_io_ops_table;
	#endif
").

:- pragma c_code("
	Word ML_io_stream_names;
	Word ML_io_user_globals;
	#if 0
	  extern Word ML_io_ops_table;
	#endif
").

:- type io__stream_names ==	map(io__stream, string).
:- type io__stream_putback ==	map(io__stream, list(char)).

:- type io__input_stream ==	io__stream.
:- type io__output_stream ==	io__stream.

:- type io__binary_stream ==	io__stream.

:- type io__stream == c_pointer.

/*
 * In NU-Prolog: 
 *	io__stream	--->	stream(int, int)
 *			;	user_input
 *			;	user_output
 *			;	user_error.
 * In C:
 *	io__stream	==	pointer to MercuryFile (which is defined
 *				in runtime/mercury_library_types.h)
 */

	% This inter-language stuff is tricky.
	% We communicate via ints rather than via io__result_codes because
	% we don't want the C code to depend on how Mercury stores its
	% discriminated union data types.

:- pred io__read_char_code(io__input_stream, int, io__state, io__state).
:- mode io__read_char_code(in, out, di, uo) is det.
%		Reads a character code from specified stream.
%		Returns -1 if at EOF, -2 if an error occurs.

:- pred io__call_system_code(string, int, io__state, io__state).
:- mode io__call_system_code(in, out, di, uo) is det.
%	io__call_system_code(Command, Status, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Returns Status = 127 on failure.  Otherwise
%		returns the exit status as a positive integer, or the
%		signal which killed the command as a negative integer.

:- pred io__do_open(string, string, int, io__input_stream,
			io__state, io__state).
:- mode io__do_open(in, in, out, out, di, uo) is det.
%	io__do_open(File, Mode, ResultCode, Stream, IO0, IO1).
%		Attempts to open a file in the specified mode.
%		Result is 0 for success, -1 for failure.

:- semipure pred io__getenv(string, string).
:- mode io__getenv(in, out) is semidet.
%	io__getenv(Var, Value).
%		Gets the value Value associated with the environment
%		variable Var.  Fails if the variable was not set.

:- impure pred io__putenv(string).
:- mode io__putenv(in) is semidet.
%	io__putenv(VarString).
%		If VarString is a string of the form "name=value",
%		sets the environment variable name to the specified
%		value.  Fails if the operation does not work.

%-----------------------------------------------------------------------------%

% input predicates

io__read_char(Result) -->
	io__input_stream(Stream),
	io__read_char(Stream, Result).

io__read_char(Stream, Result) -->
	io__read_char_code(Stream, Code),
	(
		{ Code = -1 }
	->
		{ Result = eof }
	;
		{ char__to_int(Char, Code) }
	->
		{ Result = ok(Char) }
	;
		io__make_err_msg("read failed: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__read_byte(Result) -->
	io__binary_input_stream(Stream),
	io__read_byte(Stream, Result).

io__read_byte(Stream, Result) -->
	io__read_char_code(Stream, Code),
	(
		{ Code >= 0 }
	->
		{ Result = ok(Code) }
	;
		{ Code = -1 }
	->
		{ Result = eof }
	;
		io__make_err_msg("read failed: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__read_word(Result) -->
	io__input_stream(Stream),
	io__read_word(Stream, Result).
	
io__read_word(Stream, Result) -->
	io__ignore_whitespace(Stream, WSResult),
	(
		{ WSResult = error(Error) },
		{ Result = error(Error) }
	;
		{ WSResult = eof },
		{ Result = eof }
	;
		{ WSResult = ok },
		io__read_word_2(Stream, Result)
	).

:- pred io__read_word_2(io__input_stream, io__result(list(char)),
				io__state, io__state).
:- mode	io__read_word_2(in, out, di, uo) is det.

io__read_word_2(Stream, Result) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		(
			{ char__is_whitespace(Char) }
		->
			io__putback_char(Stream, Char),
			{ Result = ok([]) }
		;
			io__read_word_2(Stream, Result0),
			(
				{ Result0 = ok(Chars) },
				{ Result = ok([Char | Chars]) }
			;
				{ Result0 = error(_) },
				{ Result = Result0 }
			;
				{ Result0 = eof },
				{ Result = ok([Char]) }
			)
		)	
	).

io__read_line(Result) -->
	io__input_stream(Stream),
	io__read_line(Stream, Result).

io__read_line(Stream, Result) -->
	io__read_char_code(Stream, Code),
	(
		{ Code = -1 }
	->
		{ Result = eof }
	;
		{ char__to_int(Char, Code) }
	->
		( { Char = '\n' } ->
			{ Result = ok([Char]) }
		;
			io__read_line_2(Stream, Result0),
			{ Result = ok([Char | Result0]) }
		)
	;
		io__make_err_msg("read failed: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

:- pred io__read_line_2(io__input_stream, list(char), io__state, io__state).
:- mode io__read_line_2(in, out, di, uo) is det.

io__read_line_2(Stream, Result) -->
	io__read_char_code(Stream, Code),
	(
		{ Code = -1 }
	->
		{ Result = [] }
	;
		{ char__to_int(Char, Code) }
	->
		( { Char = '\n' } ->
			{ Result = [Char] }
		;
			io__read_line_2(Stream, Chars),
			{ Result = [Char | Chars] }
		)
	;
		{ Result = [] }
	).

io__read_line_as_string(Result) -->
	io__input_stream(Stream),
	io__read_line_as_string(Stream, Result).

io__read_line_as_string(Stream, Result, IO0, IO) :-
	io__read_line_as_string_2(Stream, Res, String, IO0, IO1),
	( Res < 0 ->
		( Res = -1 ->
			Result = eof,
			IO = IO1
		;
			io__make_err_msg("read failed: ", Msg, IO1, IO),
			Result = error(io_error(Msg))
		)
	;
		Result = ok(String),
		IO = IO1
	).

:- pred io__read_line_as_string_2(io__input_stream, int, string,
		io__state, io__state).
:- mode io__read_line_as_string_2(in, out, out, di, uo) is det.

:- pragma c_code(io__read_line_as_string_2(File::in, Res :: out,
			RetString::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe],
"
#define ML_IO_READ_LINE_GROW(n)	((n) * 3 / 2)
#define ML_IO_BYTES_TO_WORDS(n)	(((n) + sizeof(Word) - 1) / sizeof(Word))
#define ML_IO_READ_LINE_START	1024

	Char initial_read_buffer[ML_IO_READ_LINE_START];
	Char *read_buffer = initial_read_buffer;
	size_t read_buf_size = ML_IO_READ_LINE_START;
	size_t i;
	int char_code = '\\0';

	Res = 0;
	for (i = 0; char_code != '\\n'; ) {
		char_code = mercury_getc((MercuryFile *) File);
		if (char_code == EOF) {
			if (i == 0) {
				Res = -1;
			}
			break;
		}
		if (char_code != (Char) char_code) {
			Res = -2;
			break;
		}
		read_buffer[i++] = char_code;
		MR_assert(i <= read_buf_size);
		if (i == read_buf_size) {
			/* Grow the read buffer */
			read_buf_size = ML_IO_READ_LINE_GROW(read_buf_size);
			if (read_buffer == initial_read_buffer) {
				read_buffer = checked_malloc(read_buf_size
						* sizeof(Char));
				memcpy(read_buffer, initial_read_buffer,
					ML_IO_READ_LINE_START);
			} else {
				read_buffer = checked_realloc(read_buffer,
						read_buf_size * sizeof(Char));
			}
		}
	}
	if (Res == 0) {
		incr_hp_atomic_msg(LVALUE_CAST(Word, RetString),
			ML_IO_BYTES_TO_WORDS((i + 1) * sizeof(Char)),
			mercury__io__read_line_as_string_2_5_0,
			""string:string/0"");
		memcpy(RetString, read_buffer, i * sizeof(Char));
		RetString[i] = '\\0';
	} else {
		RetString = NULL;
	}
	if (read_buffer != initial_read_buffer) {
		free(read_buffer);
	}
	update_io(IO0, IO);
").

io__read_file(Result) -->
	io__input_stream(Stream),
	io__read_file(Stream, Result).

io__read_file(Stream, Result) -->
	io__read_file_2(Stream, [], Result).

:- pred io__read_file_2(io__input_stream, list(char), io__result(list(char)),
		io__state, io__state).
:- mode io__read_file_2(in, in, out, di, uo) is det.

io__read_file_2(Stream, Chars0, Result) -->
	io__read_char(Stream, Result0),
	(
		{ Result0 = eof },
		{ list__reverse(Chars0, Chars) },
		{ Result = ok(Chars) }
	;
		{ Result0 = error(Err) },
		{ Result = error(Err) }
	;
		{ Result0 = ok(Char) },
		io__read_file_2(Stream, [Char|Chars0], Result)
	).

%-----------------------------------------------------------------------------%

io__read_file_as_string(Result, String) -->
	io__input_stream(Stream),
	io__read_file_as_string(Stream, Result, String).

io__read_file_as_string(Stream, Result, String) -->
	%
	% check if the stream is a regular file;
	% if so, allocate a buffer according to the
	% size of the file.  Otherwise, just use
	% a default buffer size of 4k minus a bit
	% (to give malloc some room).
	%
	io__stream_file_size(Stream, FileSize),
	{ FileSize >= 0 ->
		BufferSize0 is FileSize + 1
	;
		BufferSize0 = 4000
	},
	{ io__alloc_buffer(BufferSize0, Buffer0) },

	%
	% Read the file into the buffer (resizing it as we go if necessary),
	% convert the buffer into a string, and see if anything went wrong.
	%
	io__clear_err(Stream),
	{ Pos0 = 0 },
	io__read_file_as_string_2(Stream, Buffer0, Pos0, BufferSize0,
		Buffer, Pos, BufferSize),
	{ require(Pos < BufferSize, "io__read_file_as_string: overflow") },
	{ io__buffer_to_string(Buffer, Pos, String) },
	io__check_err(Stream, Result).

:- pred io__read_file_as_string_2(io__input_stream, buffer, int, int,
		buffer, int, int, io__state, io__state).
:- mode io__read_file_as_string_2(in, di, in, in,
		uo, out, out, di, uo) is det.

io__read_file_as_string_2(Stream, Buffer0, Pos0, Size0, Buffer, Pos, Size) -->
	io__read_into_buffer(Stream, Buffer0, Pos0, Size0,
		Buffer1, Pos1),
	( { Pos1 = Pos0 } ->
		% end of file (or error)
		{ Size = Size0 },
		{ Pos = Pos1 },
		{ Buffer = Buffer1 }
	; { Pos1 = Size0 } ->
		% full buffer
		{ Size1 is Size0 * 2 },
		{ io__resize_buffer(Buffer1, Size0, Size1, Buffer2) },
		io__read_file_as_string_2(Stream, Buffer2, Pos1, Size1,
			Buffer, Pos, Size)
	;
		io__read_file_as_string_2(Stream, Buffer1, Pos1, Size0,
			Buffer, Pos, Size)
	).
	
%-----------------------------------------------------------------------------%

:- pred io__clear_err(stream, io__state, io__state).
:- mode io__clear_err(in, di, uo) is det.
% same as ANSI C's clearerr().

:- pragma c_code(io__clear_err(Stream::in, _IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;
	clearerr(f->file);
}").

:- pred io__check_err(stream, io__res, io__state, io__state).
:- mode io__check_err(in, out, di, uo) is det.

io__check_err(Stream, Res) -->
	io__ferror(Stream, Int, Msg),
	{ Int = 0 ->
		Res = ok
	;
		Res = error(io_error(Msg))
	}.

:- pred io__ferror(stream, int, string, io__state, io__state).
:- mode io__ferror(in, out, out, di, uo) is det.
% similar to ANSI C's ferror().

:- pragma c_code(ferror(Stream::in, RetVal::out, RetStr::out,
		_IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;
	RetVal = ferror(f->file);
	ML_maybe_make_err_msg(RetVal != 0, ""read failed: "",
		mercury__io__ferror_5_0, RetStr);
}").

% io__make_err_msg(MessagePrefix, Message):
%	`Message' is an error message obtained by looking up the
%	message for the current value of errno and prepending
%	`MessagePrefix'.
:- pred io__make_err_msg(string, string, io__state, io__state).
:- mode io__make_err_msg(in, out, di, uo) is det.

:- pragma c_code(make_err_msg(Msg0::in, Msg::out, _IO0::di, _IO::uo),
		will_not_call_mercury,
"{
	ML_maybe_make_err_msg(TRUE, Msg0, mercury__io__make_err_msg_4_0,
		Msg);
}").

%-----------------------------------------------------------------------------%

:- pred io__stream_file_size(stream, int, io__state, io__state).
:- mode io__stream_file_size(in, out, di, uo) is det.
% io__stream_file_size(Stream, Size):
%	if Stream is a regular file, then Size is its size (in bytes),
%	otherwise Size is -1.

:- pragma c_header_code("
	#include <unistd.h>
#ifdef HAVE_SYS_STAT_H
	#include <sys/stat.h>
#endif
").

:- pragma c_code(io__stream_file_size(Stream::in, Size::out,
		_IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;
#if defined(HAVE_FSTAT) && \
    (defined(HAVE_FILENO) || defined(fileno)) && \
    defined(S_ISREG)
	struct stat s;
	if (fstat(fileno(f->file), &s) == 0 && S_ISREG(s.st_mode)) {
		Size = s.st_size;
	} else {
		Size = -1;
	}
#else
	Size = -1;
#endif
}").

%-----------------------------------------------------------------------------%

% A `buffer' is just an array of Chars.
% Buffer sizes are measured in Chars.

:- type buffer ---> buffer(c_pointer).

:- pred io__alloc_buffer(int::in, buffer::uo) is det.
:- pragma c_code(io__alloc_buffer(Size::in, Buffer::uo),
		[will_not_call_mercury, thread_safe],
"{
	incr_hp_atomic_msg(Buffer,
		(Size * sizeof(Char) + sizeof(Word) - 1) / sizeof(Word),
		mercury__io__alloc_buffer_2_0,
		""io:buffer/0"");
}").

:- pred io__resize_buffer(buffer::di, int::in, int::in, buffer::uo) is det.
:- pragma c_code(io__resize_buffer(Buffer0::di, OldSize::in, NewSize::in,
			Buffer::uo),
	[will_not_call_mercury, thread_safe],
"{
	Char *buffer0 = (Char *) Buffer0;
	Char *buffer;

#ifdef CONSERVATIVE_GC
	buffer = GC_REALLOC(buffer0, NewSize * sizeof(Char));
#else
	if (buffer0 + OldSize == (Char *) MR_hp) {
		Word next;
		incr_hp_atomic_msg(next, 
		   (NewSize * sizeof(Char) + sizeof(Word) - 1) / sizeof(Word),
		   mercury__io__resize_buffer_4_0,
		   ""io:buffer/0"");
		assert(buffer0 + OldSize == (Char *) next);
	    	buffer = buffer0;
	} else {
		/* just have to alloc and copy */
		incr_hp_atomic_msg(Buffer,
		   (NewSize * sizeof(Char) + sizeof(Word) - 1) / sizeof(Word),
		   mercury__io__resize_buffer_4_0,
		   ""io:buffer/0"");
		buffer = (Char *) Buffer;
		if (OldSize > NewSize) {
			memcpy(buffer, buffer0, NewSize);
		} else {
			memcpy(buffer, buffer0, OldSize);
		}
	}
#endif

	Buffer = (Word) buffer;
}").

:- pred io__buffer_to_string(buffer::di, int::in, string::uo) is det.
:- pragma c_code(io__buffer_to_string(Buffer::di, Len::in, Str::uo),
	[will_not_call_mercury, thread_safe],
"{
	Str = (String) Buffer;
	Str[Len] = '\\0';
}").

:- pred io__buffer_to_string(buffer::di, string::uo) is det.
:- pragma c_code(io__buffer_to_string(Buffer::di, Str::uo),
	[will_not_call_mercury, thread_safe],
"{
	Str = (String) Buffer;
}").

:- pred io__read_into_buffer(stream::in, buffer::di, int::in, int::in,
		    buffer::uo, int::out, io__state::di, io__state::uo) is det.

:- pragma c_code(io__read_into_buffer(Stream::in,
		    Buffer0::di, Pos0::in, Size::in,
		    Buffer::uo, Pos::out, _IO0::di, _IO::uo),
		[will_not_call_mercury, thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;
	char *buffer = (Char *) Buffer0;
	int items_read;

	items_read = fread(buffer + Pos0, sizeof(Char), Size - Pos0, f->file);

	Buffer = (Word) buffer;
	Pos = Pos0 + items_read;
}").

%-----------------------------------------------------------------------------%

io__read_binary_file(Result) -->
	io__binary_input_stream(Stream),
	io__read_binary_file(Stream, Result).

io__read_binary_file(Stream, Result) -->
	io__read_binary_file_2(Stream, [], Result).

:- pred io__read_binary_file_2(io__input_stream, list(int),
		io__result(list(int)), io__state, io__state).
:- mode io__read_binary_file_2(in, in, out, di, uo) is det.

io__read_binary_file_2(Stream, Bytes0, Result) -->
	io__read_byte(Stream, Result0),
	(
		{ Result0 = eof },
		{ list__reverse(Bytes0, Bytes) },
		{ Result = ok(Bytes) }
	;
		{ Result0 = error(Err) },
		{ Result = error(Err) }
	;
		{ Result0 = ok(Byte) },
		io__read_binary_file_2(Stream, [Byte|Bytes0], Result)
	).

io__putback_char(Char) -->
	io__input_stream(Stream),
	io__putback_char(Stream, Char).

io__putback_byte(Char) -->
	io__binary_input_stream(Stream),
	io__putback_byte(Stream, Char).

io__read_anything(Result) -->
	io__read(Result).

io__read(Result) -->
	term_io__read_term(ReadResult),
	io__get_line_number(LineNumber),
	{ io__process_read_term(ReadResult, LineNumber, Result) }.

io__read_from_string(FileName, String, Len, Result, Posn0, Posn) :-
	parser__read_term_from_string(FileName, String, Len, Posn0, Posn, ReadResult),
	Posn = posn(LineNumber, _, _),
	io__process_read_term(ReadResult, LineNumber, Result).

:- pred io__process_read_term(read_term, int, io__read_result(T)).
:- mode io__process_read_term(in, in, out) is det.

io__process_read_term(ReadResult, LineNumber, Result) :-
	(	
		ReadResult = term(_VarSet, Term),
		( term_to_type(Term, Type) ->
			Result = ok(Type)
		;
			( \+ term__is_ground(Term) ->
				Result = error("io__read: the term read was not a ground term",
					LineNumber)
			;
				Result = error(
					"io__read: the term read did not have the right type",
					LineNumber)
			)
		)
	;
		ReadResult = eof,
		Result = eof
	;
		ReadResult = error(String, Int),
		Result = error(String, Int)
	).

io__read_anything(Stream, Result) -->
	io__read(Stream, Result).

io__read(Stream, Result) -->
	io__set_input_stream(Stream, OrigStream),
	io__read(Result),
	io__set_input_stream(OrigStream, _Stream).

io__ignore_whitespace(Result) -->
	io__input_stream(Stream),
	io__ignore_whitespace(Stream, Result).

io__ignore_whitespace(Stream, Result) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		(
			{ char__is_whitespace(Char) }
		->
			io__ignore_whitespace(Stream, Result)
		;
			io__putback_char(Stream, Char),
			{ Result = ok }
		)	
	).

%-----------------------------------------------------------------------------%

% output predicates

io__nl -->
	io__write_char('\n').

io__nl(Stream) -->
	io__write_char(Stream, '\n').

io__write_strings(Strings) -->
	io__output_stream(Stream),
	io__write_strings(Stream, Strings).

io__write_strings(_Stream, []) --> [].
io__write_strings(Stream, [S|Ss]) -->
	io__write_string(Stream, S),
	io__write_strings(Stream, Ss).

io__format(FormatString, Arguments) -->
	io__output_stream(Stream),
	io__format(Stream, FormatString, Arguments).

io__format(Stream, FormatString, Arguments) -->
	{ string__format(FormatString, Arguments, String) },
	io__write_string(Stream, String).

io__write_many(Poly_list) -->
	io__output_stream(Stream),
	io__write_many(Stream, Poly_list).

io__write_many( _Stream, [], IO, IO ).
io__write_many( Stream, [ c(C) | Rest ] ) -->
	io__write_char(Stream, C),
	io__write_many(Stream, Rest).
io__write_many( Stream, [ i(I) | Rest ] ) -->
	io__write_int(Stream, I),
	io__write_many(Stream, Rest).
io__write_many( Stream, [ s(S) | Rest ]) -->
	io__write_string(Stream, S),
	io__write_many(Stream, Rest).
io__write_many( Stream, [ f(F) | Rest ]) -->
	io__write_float(Stream, F),
	io__write_many(Stream, Rest).

:- pragma export(io__print(in, in, di, uo), "ML_io_print_to_stream").

io__print(Stream, Term) -->
	io__set_output_stream(Stream, OrigStream),
	io__print(Term),
	io__set_output_stream(OrigStream, _Stream).

:- pragma export(io__print(in, di, uo), "ML_io_print_to_cur_stream").

io__print(Term) -->
	% `string', `char' and `univ' are special cases for io__print
	{ type_to_univ(Term, Univ) },
	( { univ_to_type(Univ, String) } ->
		io__write_string(String)
	; { univ_to_type(Univ, Char) } ->
		io__write_char(Char)
	; { univ_to_type(Univ, OrigUniv) } ->
		io__write_univ(OrigUniv)
	;
		io__print_quoted(Term)
	).

:- pred io__print_quoted(T, io__state, io__state).
:- mode io__print_quoted(in, di, uo) is det.

io__print_quoted(Term) -->
	io__write(Term).
/*
When we have type classes, then instead of io__write(Term),
we will want to do something like
	( { univ_to_type_class(Univ, Portrayable) } ->
		portray(Portrayable)
	;
		... code like io__write, but which prints
		    the arguments using io__print_quoted, rather than
		    io__write ...
	)
*/

io__write_anything(Anything) -->
	io__write(Anything).

io__write_anything(Stream, Anything) -->
	io__write(Stream, Anything).

io__write(Stream, X) -->
	io__set_output_stream(Stream, OrigStream),
	io__write(X),
	io__set_output_stream(OrigStream, _Stream).

%-----------------------------------------------------------------------------%

io__write(Term) -->
	{ type_to_univ(Term, Univ) },
	io__write_univ(Univ).

io__write_univ(Univ) -->
	{ ops__max_priority(MaxPriority) },
	io__write_univ(Univ, MaxPriority + 1).

:- pred io__write_univ(univ, ops__priority, io__state, io__state).
:- mode io__write_univ(in, in, di, uo) is det.

io__write_univ(Univ, Priority) -->
	%
	% we need to special-case the builtin types:
	%	int, char, float, string
	%	type_info, univ, c_pointer, array
	%	and private_builtin:type_info
	%
	( { univ_to_type(Univ, String) } ->
		term_io__quote_string(String)
	; { univ_to_type(Univ, Char) } ->
		term_io__quote_char(Char)
	; { univ_to_type(Univ, Int) } ->
		io__write_int(Int)
	; { univ_to_type(Univ, Float) } ->
		io__write_float(Float)
	; { univ_to_type(Univ, TypeInfo) } ->
		io__write_type_info(TypeInfo)
	; { univ_to_type(Univ, OrigUniv) } ->
		io__write_univ_as_univ(OrigUniv)
	; { univ_to_type(Univ, C_Pointer) } ->
		io__write_c_pointer(C_Pointer)
	;
		%
		% Check if the type is array:array/1.
		% We can't just use univ_to_type here since 
		% array:array/1 is a polymorphic type.
		%
		% The calls to type_ctor_name and type_ctor_module_name
		% are not really necessary -- we could use univ_to_type
		% in the condition instead of det_univ_to_type in the body.
		% However, this way of doing things is probably more efficient
		% in the common case when the thing being printed is
		% *not* of type array:array/1.
		%
		% The ordering of the tests here (arity, then name, then
		% module name, rather than the reverse) is also chosen
		% for efficiency, to find failure cheaply in the common cases,
		% rather than for readability.
		%
		{ type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes) },
		{ ArgTypes = [ElemType] },
		{ type_ctor_name(TypeCtor) = "array" },
		{ type_ctor_module_name(TypeCtor) = "array" }
	->
		%
		% Now that we know the element type, we can
		% constrain the type of the variable `Array'
		% so that we can use det_univ_to_type.
		%
		{ has_type(Elem, ElemType) },
		{ same_array_elem_type(Array, Elem) },
		{ det_univ_to_type(Univ, Array) },
		io__write_array(Array)
	; 
		%
		% Check if the type is private_builtin:type_info/1.
		% See the comments above for array:array/1.
		%
		{ type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes) },
		{ ArgTypes = [ElemType] },
		{ type_ctor_name(TypeCtor) = "type_info" },
		{ type_ctor_module_name(TypeCtor) = "private_builtin" }
	->
		{ has_type(Elem, ElemType) },
		{ same_private_builtin_type(PrivateBuiltinTypeInfo, Elem) },
		{ det_univ_to_type(Univ, PrivateBuiltinTypeInfo) },
		io__write_private_builtin_type_info(PrivateBuiltinTypeInfo)
	; 
		io__write_ordinary_term(Univ, Priority)
	).

:- pred same_array_elem_type(array(T), T).
:- mode same_array_elem_type(unused, unused) is det.
same_array_elem_type(_, _).

:- pred same_private_builtin_type(private_builtin__type_info(T), T).
:- mode same_private_builtin_type(unused, unused) is det.
same_private_builtin_type(_, _).


:- pred io__write_ordinary_term(univ, ops__priority, io__state, io__state).
:- mode io__write_ordinary_term(in, in, di, uo) is det.

io__write_ordinary_term(Term, Priority) -->
	{ deconstruct(Term, Functor, _Arity, Args) },
	io__get_op_table(OpTable),
	(
		{ Functor = "." },
		{ Args = [ListHead, ListTail] }
	->
		io__write_char('['),
		io__write_arg(ListHead),
		io__write_list_tail(ListTail),
		io__write_char(']')
	;
		{ Functor = "[]" },
		{ Args = [] }
	->
		io__write_string("[]")
	;
		{ Functor = "{}" },
		{ Args = [BracedTerm] }
	->
		io__write_string("{ "),
		io__write_univ(BracedTerm),
		io__write_string(" }")
	;
		{ Args = [PrefixArg] },
		{ ops__lookup_prefix_op(OpTable, Functor,
			OpPriority, OpAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		term_io__quote_atom(Functor),
		io__write_char(' '),
		{ adjust_priority(OpPriority, OpAssoc, NewPriority) },
		io__write_univ(PrefixArg, NewPriority),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [PostfixArg] },
		{ ops__lookup_postfix_op(OpTable, Functor,
			OpPriority, OpAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		{ adjust_priority(OpPriority, OpAssoc, NewPriority) },
		io__write_univ(PostfixArg, NewPriority),
		io__write_char(' '),
		term_io__quote_atom(Functor),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [Arg1, Arg2] },
		{ ops__lookup_infix_op(OpTable, Functor, 
			OpPriority, LeftAssoc, RightAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		{ adjust_priority(OpPriority, LeftAssoc, LeftPriority) },
		io__write_univ(Arg1, LeftPriority),
		( { Functor = "," } ->
			io__write_string(", ")
		;
			io__write_char(' '),
			term_io__quote_atom(Functor),
			io__write_char(' ')
		),
		{ adjust_priority(OpPriority, RightAssoc, RightPriority) },
		io__write_univ(Arg2, RightPriority),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [Arg1, Arg2] },
		{ ops__lookup_binary_prefix_op(OpTable, Functor,
			OpPriority, FirstAssoc, SecondAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		term_io__quote_atom(Functor),
		io__write_char(' '),
		{ adjust_priority(OpPriority, FirstAssoc, FirstPriority) },
		io__write_univ(Arg1, FirstPriority),
		io__write_char(' '),
		{ adjust_priority(OpPriority, SecondAssoc, SecondPriority) },
		io__write_univ(Arg2, SecondPriority),
		maybe_write_char(')', Priority, OpPriority)
	;
		(
			{ Args = [] },
			{ ops__lookup_op(OpTable, Functor) },
			{ ops__max_priority(MaxPriority) },
			{ Priority =< MaxPriority }
		->
			io__write_char('('),
			term_io__quote_atom(Functor),
			io__write_char(')')
		;
			term_io__quote_atom(Functor,
				maybe_adjacent_to_graphic_token)
		),
		(
			{ Args = [X|Xs] }
		->
			io__write_char('('),
			io__write_arg(X),
			io__write_term_args(Xs),
			io__write_char(')')
		;
			[]
		)
	).

:- pred maybe_write_char(char, ops__priority, ops__priority,
			io__state, io__state).
:- mode maybe_write_char(in, in, in, di, uo) is det.

maybe_write_char(Char, Priority, OpPriority) -->
	( { OpPriority > Priority } ->
		io__write_char(Char)
	;
		[]
	).

:- pred adjust_priority(ops__priority, ops__assoc, ops__priority).
:- mode adjust_priority(in, in, out) is det.

adjust_priority(Priority, y, Priority).
adjust_priority(Priority, x, Priority - 1).

:- pred io__write_list_tail(univ, io__state, io__state).
:- mode io__write_list_tail(in, di, uo) is det.

io__write_list_tail(Term) -->
	( 
		{ deconstruct(Term, ".", _Arity, [ListHead, ListTail]) }
	->
		io__write_string(", "),
		io__write_arg(ListHead),
		io__write_list_tail(ListTail)
	;
		{ deconstruct(Term, "[]", _Arity, []) }
	->
		[]
	;
		io__write_string(" | "),
		io__write_univ(Term)
	).

:- pred io__write_term_args(list(univ), io__state, io__state).
:- mode io__write_term_args(in, di, uo) is det.

	% write the remaining arguments
io__write_term_args([]) --> [].
io__write_term_args([X|Xs]) -->
	io__write_string(", "),
	io__write_arg(X),
	io__write_term_args(Xs).

:- pred io__write_arg(univ, io__state, io__state).
:- mode io__write_arg(in, di, uo) is det.

io__write_arg(X) -->
	arg_priority(ArgPriority),
	io__write_univ(X, ArgPriority).

:- pred arg_priority(int, io__state, io__state).
:- mode arg_priority(out, di, uo) is det.
/*
arg_priority(ArgPriority) -->
	io__get_op_table(OpTable),
	{ ops__lookup_infix_op(OpTable, ",", Priority, _, _) ->
		ArgPriority = Priority }
	;
		error("arg_priority: can't find the priority of `,'")
	}.
*/
% We could implement this as above, but it's more efficient to just
% hard-code it.
arg_priority(1000) --> [].

%-----------------------------------------------------------------------------%

:- pred io__write_type_info(type_info, io__state, io__state).
:- mode io__write_type_info(in, di, uo) is det.

io__write_type_info(TypeInfo) -->
	io__write_string(type_name(TypeInfo)).

:- pred io__write_univ_as_univ(univ, io__state, io__state).
:- mode io__write_univ_as_univ(in, di, uo) is det.

io__write_univ_as_univ(Univ) -->
	io__write_string("univ("),
	io__write_univ(Univ),
	% XXX what is the right TYPE_QUAL_OP to use here?
	io__write_string(" : "),
	io__write_string(type_name(univ_type(Univ))),
	io__write_string(")").

:- pred io__write_c_pointer(c_pointer, io__state, io__state).
:- mode io__write_c_pointer(in, di, uo) is det.

io__write_c_pointer(_C_Pointer) -->
	% XXX what should we do here?
	io__write_string("'<<c_pointer>>'").

:- pred io__write_array(array(T), io__state, io__state).
:- mode io__write_array(in, di, uo) is det.

io__write_array(Array) -->
	io__write_string("array("),
	{ array__to_list(Array, List) },
	io__write(List),
	io__write_string(")").

:- pred io__write_private_builtin_type_info(private_builtin__type_info(T)::in,
		io__state::di, io__state::uo) is det.
io__write_private_builtin_type_info(PrivateBuiltinTypeInfo) -->
	{ TypeInfo = unsafe_cast(PrivateBuiltinTypeInfo) },
	io__write_type_info(TypeInfo).

:- func unsafe_cast(T1::in) = (T2::out) is det.
:- pragma c_code(unsafe_cast(VarIn::in) = (VarOut::out),
	[will_not_call_mercury, thread_safe],
"
	VarOut = VarIn;
").

%-----------------------------------------------------------------------------%

io__write_list([], _Separator, _OutputPred) --> [].
io__write_list([E|Es], Separator, OutputPred) -->
	call(OutputPred, E),
	(
		{ Es = [] }
	;
		{ Es = [_|_] },
		io__write_string(Separator)
	),
	io__write_list(Es, Separator, OutputPred).

io__write_list(Stream, List, Separator, OutputPred) -->
	io__set_output_stream(Stream, OrigStream),
	io__write_list(List, Separator, OutputPred),
	io__set_output_stream(OrigStream, _Stream).

%-----------------------------------------------------------------------------%

io__write_binary(Stream, Term) -->
	io__set_binary_output_stream(Stream, OrigStream),
	io__write_binary(Term),
	io__set_binary_output_stream(OrigStream, _Stream).

io__read_binary(Stream, Result) -->
	io__set_binary_input_stream(Stream, OrigStream),
	io__read_binary(Result),
	io__set_binary_input_stream(OrigStream, _Stream).

io__write_binary(Term) -->
	% a quick-and-dirty implementation... not very space-efficient
	% (not really binary!)
	io__binary_output_stream(Stream),
	io__write(Stream, Term),
	io__write_string(Stream, ".\n").

io__read_binary(Result) -->
	% a quick-and-dirty implementation... not very space-efficient
	% (not really binary!)
	io__binary_input_stream(Stream),
	io__read(Stream, ReadResult),
	{ io__convert_read_result(ReadResult, Result) }.

:- pred io__convert_read_result(io__read_result(T), io__result(T)).
:- mode io__convert_read_result(in, out) is det.

io__convert_read_result(ok(T), ok(T)).
io__convert_read_result(eof, eof).
io__convert_read_result(error(Error, _Line), error(io_error(Error))).

%-----------------------------------------------------------------------------%

% stream predicates

io__open_input(FileName, Result) -->
	io__do_open(FileName, "r", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		io__make_err_msg("can't open input file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_output(FileName, Result) -->
	io__do_open(FileName, "w", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		io__make_err_msg("can't open output file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_append(FileName, Result) -->
	io__do_open(FileName, "a", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		io__make_err_msg("can't append to file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_binary_input(FileName, Result) -->
	io__do_open(FileName, "rb", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		io__make_err_msg("can't open input file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_binary_output(FileName, Result) -->
	io__do_open(FileName, "wb", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		io__make_err_msg("can't open output file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_binary_append(FileName, Result) -->
	io__do_open(FileName, "ab", Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		io__make_err_msg("can't append to file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's see/1 and seen/0.

io__see(File, Result) -->
	io__open_input(File, Result0),
	(
		{ Result0 = ok(Stream) },
		io__set_input_stream(Stream, _),
		{ Result = ok }
	;
		{ Result0 = error(Error) },
		{ Result = error(Error) }
	).

io__seen -->
	io__stdin_stream(Stdin),
	io__set_input_stream(Stdin, OldStream),
	io__close_input(OldStream).

	% Plus binary IO versions.

io__see_binary(File, Result) -->
	io__open_binary_input(File, Result0),
	(
		{ Result0 = ok(Stream) },
		io__set_binary_input_stream(Stream, _),
		{ Result = ok }
	;
		{ Result0 = error(Error) },
		{ Result = error(Error) }
	).

io__seen_binary -->
	io__stdin_binary_stream(Stdin),
	io__set_binary_input_stream(Stdin, OldStream),
	io__close_binary_input(OldStream).

%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's tell/1 and told/0.

io__told -->
	io__stdout_stream(Stdout),
	io__set_output_stream(Stdout, OldStream),
	io__close_output(OldStream).

io__tell(File, Result) -->
	io__open_output(File, Result0),
	(
		{ Result0 = ok(Stream) },
		io__set_output_stream(Stream, _),
		{ Result = ok }
	;
		{ Result0 = error(Msg) },
		{ Result = error(Msg) }
	).

io__told_binary -->
	io__stdout_binary_stream(Stdout),
	io__set_binary_output_stream(Stdout, OldStream),
	io__close_binary_output(OldStream).

io__tell_binary(File, Result) -->
	io__open_binary_output(File, Result0),
	(
		{ Result0 = ok(Stream) },
		io__set_binary_output_stream(Stream, _),
		{ Result = ok }
	;
		{ Result0 = error(Msg) },
		{ Result = error(Msg) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% stream name predicates

io__input_stream_name(Name) -->
	io__input_stream(Stream),
	io__stream_name(Stream, Name).

io__input_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__output_stream_name(Name) -->
	io__output_stream(Stream),
	io__stream_name(Stream, Name).

io__output_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__binary_input_stream_name(Name) -->
	io__binary_input_stream(Stream),
	io__stream_name(Stream, Name).

io__binary_input_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

io__binary_output_stream_name(Name) -->
	io__binary_output_stream(Stream),
	io__stream_name(Stream, Name).

io__binary_output_stream_name(Stream, Name) -->
	io__stream_name(Stream, Name).

:- pred io__stream_name(io__stream, string, io__state, io__state).
:- mode io__stream_name(in, out, di, uo) is det.

io__stream_name(Stream, Name) -->
	io__get_stream_names(StreamNames),
	{ map__search(StreamNames, Stream, Name1) ->
		Name = Name1
	;
		Name = "<stream name unavailable>"
	},
	io__set_stream_names(StreamNames).

:- pred io__get_stream_names(io__stream_names, io__state, io__state).
:- mode io__get_stream_names(out, di, uo) is det.

:- pragma c_code(io__get_stream_names(StreamNames::out, IO0::di, IO::uo), 
		will_not_call_mercury, "
	StreamNames = ML_io_stream_names;
	update_io(IO0, IO);
").

:- pred io__set_stream_names(io__stream_names, io__state, io__state).
:- mode io__set_stream_names(in, di, uo) is det.

:- pragma c_code(io__set_stream_names(StreamNames::in, IO0::di, IO::uo), 
		will_not_call_mercury, "
	ML_io_stream_names = StreamNames;
	update_io(IO0, IO);
").

:- pred io__delete_stream_name(io__stream, io__state, io__state).
:- mode io__delete_stream_name(in, di, uo) is det.

io__delete_stream_name(Stream) -->
	io__get_stream_names(StreamNames0),
	{ map__delete(StreamNames0, Stream, StreamNames) },
	io__set_stream_names(StreamNames).

:- pred io__insert_stream_name(io__stream, string, io__state, io__state).
:- mode io__insert_stream_name(in, in, di, uo) is det.

io__insert_stream_name(Stream, Name) -->
	io__get_stream_names(StreamNames0),
	{ map__set(StreamNames0, Stream, Name, StreamNames) },
	io__set_stream_names(StreamNames).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% global state predicates

	% XXX design flaw with regard to unique modes
	% and io__get_globals/3: the `Globals::uo' mode here is a lie.

:- pragma c_code(io__get_globals(Globals::uo, IOState0::di, IOState::uo), 
		will_not_call_mercury, "
	Globals = ML_io_user_globals;
	update_io(IOState0, IOState);
").

:- pragma c_code(io__set_globals(Globals::di, IOState0::di, IOState::uo), 
		will_not_call_mercury, "
	/* XXX need to globalize the memory */
	ML_io_user_globals = Globals;
	update_io(IOState0, IOState);
").

io__progname_base(DefaultName, PrognameBase) -->
	io__progname(DefaultName, Progname),
	{ dir__basename(Progname, PrognameBase) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% environment interface predicates

:- pragma promise_pure(io__get_environment_var/4).

io__get_environment_var(Var, OptValue) -->
	( { semipure io__getenv(Var, Value) } ->
	    { OptValue0 = yes(Value) }
	;
	    { OptValue0 = no }
	),
	{ OptValue = OptValue0 }.

:- pragma promise_pure(io__set_environment_var/4).

io__set_environment_var(Var, Value) -->
	{ string__format("%s=%s", [s(Var), s(Value)], EnvString) },
	( { impure io__putenv(EnvString) } ->
	    []
	;
	    { string__format("Could not set environment variable `%s'",
				[s(Var)], Message) },
	    { error(Message) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% memory management predicates

:- pragma promise_pure(io__report_stats/2).

io__report_stats -->
	{ impure report_stats }.

:- pragma promise_pure(io__report_full_memory_stats/2).

io__report_full_memory_stats -->
	{ impure report_full_memory_stats }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% miscellaneous predicates

:- pred io__init_state(io__state, io__state).
:- mode io__init_state(di, uo) is det.

% for use by the Mercury runtime
:- pragma export(io__init_state(di, uo), "ML_io_init_state").

io__init_state -->
	io__gc_init(type_of(StreamNames), type_of(Globals)),
	{ map__init(StreamNames) },
	{ ops__init_op_table(OpTable) },
	{ type_to_univ("<globals>", Globals) },
	io__set_stream_names(StreamNames),
	io__set_op_table(OpTable),
	io__set_globals(Globals),
	io__insert_std_stream_names.

:- pred io__finalize_state(io__state, io__state).
:- mode io__finalize_state(di, uo) is det.

% for use by the Mercury runtime
:- pragma export(io__finalize_state(di, uo), "ML_io_finalize_state").

io__finalize_state -->
	% currently no finalization needed...
	% (Perhaps we should close all open Mercury files?
	% That will happen on process exit anyway, so currently
	% we don't bother.)
	[].

:- pred io__gc_init(type_info, type_info, io__state, io__state).
:- mode io__gc_init(in, in, di, uo) is det.

:- pragma c_code(io__gc_init(StreamNamesType::in, UserGlobalsType::in,
		IO0::di, IO::uo), will_not_call_mercury, "
	/* for Windows DLLs, we need to call GC_INIT() from each DLL */
#ifdef CONSERVATIVE_GC
	GC_INIT();
#endif
	MR_add_root(&ML_io_stream_names, (Word *) StreamNamesType);
	MR_add_root(&ML_io_user_globals, (Word *) UserGlobalsType);
	update_io(IO0, IO);
").

:- pred io__insert_std_stream_names(io__state, io__state).
:- mode io__insert_std_stream_names(di, uo) is det.

io__insert_std_stream_names -->
	io__stdin_stream(Stdin),
	io__insert_stream_name(Stdin, "<standard input>"),
	io__stdout_stream(Stdout),
	io__insert_stream_name(Stdout, "<standard output>"),
	io__stderr_stream(Stderr),
	io__insert_stream_name(Stderr, "<standard error>").

io__call_system(Command, Result) -->
	io__call_system_code(Command, Status),
	{ Status = 127 ->
		% XXX improve error message
		Result = error(io_error("can't invoke system command"))
	; Status < 0 ->
		Signal is - Status,
		string__int_to_string(Signal, SignalStr),
		string__append("system command killed by signal number ",
			SignalStr, ErrMsg),
		Result = error(io_error(ErrMsg))
	;
		Result = ok(Status)
	}.

:- type io__error	
	--->	io_error(string).		% This is subject to change.
	% Note that we use `io_error' rather than `io__error'
	% because io__print, which may be called to print out the uncaught
	% exception if there is no exception hander, does not print out
	% the module name.

io__error_message(io_error(Error), Error).

%-----------------------------------------------------------------------------%

	% XXX design flaw with regard to unique modes and
	% io__get_op_table

io__get_op_table(OpTable) -->
	{ ops__init_op_table(OpTable) }.

io__set_op_table(_OpTable) --> [].

%-----------------------------------------------------------------------------%

% For use by the debugger:

:- pred io__get_io_input_stream_type(type_info, io__state, io__state).
:- mode io__get_io_input_stream_type(out, di, uo) is det.

:- pragma export(io__get_io_input_stream_type(out, di, uo),
	"ML_io_input_stream_type").

io__get_io_input_stream_type(Type) -->
	io__stdin_stream(Stream),
	{ Type = type_of(Stream) }.

:- pred io__get_io_output_stream_type(type_info, io__state, io__state).
:- mode io__get_io_output_stream_type(out, di, uo) is det.

:- pragma export(io__get_io_output_stream_type(out, di, uo),
	"ML_io_output_stream_type").

io__get_io_output_stream_type(Type) -->
	io__stdout_stream(Stream),
	{ Type = type_of(Stream) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

/*
** The remaining predicates are implemented using the C interface.
** They are also implemented for NU-Prolog in `io.nu.nl'.
*/

:- pragma c_header_code("

#include ""mercury_init.h""
#include ""mercury_wrapper.h""
#include ""mercury_type_info.h""

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

#ifdef HAVE_SYS_WAIT
  #include <sys/wait.h>		/* for WIFEXITED, WEXITSTATUS, etc. */
#endif

extern MercuryFile mercury_stdin;
extern MercuryFile mercury_stdout;
extern MercuryFile mercury_stderr;
extern MercuryFile mercury_stdin_binary;
extern MercuryFile mercury_stdout_binary;
extern MercuryFile *mercury_current_text_input;
extern MercuryFile *mercury_current_text_output;
extern MercuryFile *mercury_current_binary_input;
extern MercuryFile *mercury_current_binary_output;

#define initial_io_state()		0	/* some random number */
#define update_io(r_src, r_dest)	((r_dest) = (r_src))
#define final_io_state(r)		((void)0)

void 		mercury_init_io(void);
MercuryFile*	mercury_open(const char *filename, const char *type);
void		mercury_io_error(MercuryFile* mf, const char *format, ...);
void		mercury_output_error(MercuryFile* mf);
void		mercury_print_string(MercuryFile* mf, const char *s);
void		mercury_print_binary_string(MercuryFile* mf, const char *s);
int		mercury_getc(MercuryFile* mf);
void		mercury_close(MercuryFile* mf);
").

:- pragma c_code("

MercuryFile mercury_stdin = { NULL, 1 };
MercuryFile mercury_stdout = { NULL, 1 };
MercuryFile mercury_stderr = { NULL, 1 };
MercuryFile mercury_stdin_binary = { NULL, 1 };
MercuryFile mercury_stdout_binary = { NULL, 1 };
MercuryFile *mercury_current_text_input = &mercury_stdin;
MercuryFile *mercury_current_text_output = &mercury_stdout;
MercuryFile *mercury_current_binary_input = &mercury_stdin_binary;
MercuryFile *mercury_current_binary_output = &mercury_stdout_binary;

void
mercury_init_io(void)
{
	mercury_stdin.file = stdin;
	mercury_stdout.file = stdout;
	mercury_stderr.file = stderr;
#if defined(HAVE_FDOPEN) && (defined(HAVE_FILENO) || defined(fileno))
	mercury_stdin_binary.file = fdopen(fileno(stdin), ""rb"");
	if (mercury_stdin_binary.file == NULL) {
		fatal_error(""error opening standard input stream in ""
			""binary mode:\n\tfdopen() failed: %s"",
			strerror(errno));
	}
	mercury_stdout_binary.file = fdopen(fileno(stdout), ""wb"");
	if (mercury_stdout_binary.file == NULL) {
		fatal_error(""error opening standard output stream in ""
			""binary mode:\n\tfdopen() failed: %s"",
			strerror(errno));
	}
#else
	/*
	** XXX Standard ANSI/ISO C provides no way to set stdin/stdout
	** to binary mode.  I guess we just have to punt...
	*/
	mercury_stdin_binary.file = stdin;
	mercury_stdout_binary.file = stdout;
#endif
}

").

:- pragma c_code("

MercuryFile*
mercury_open(const char *filename, const char *type)
{
	MercuryFile *mf;
	FILE *f;
	
	f = fopen(filename, type);
	if (!f) return NULL;
	mf = make(MercuryFile);
	mf->file = f;
	mf->line_number = 1;
	return mf;
}

").

:- pred throw_io_error(string::in) is erroneous.
:- pragma export(throw_io_error(in), "ML_throw_io_error").
throw_io_error(Message) :- throw(io_error(Message)).

:- pragma c_code("

void
mercury_io_error(MercuryFile* mf, const char *format, ...)
{
	va_list args;
	char message[5000];
	ConstString message_as_mercury_string;

	/* the `mf' parameter is currently not used */

	/* format the error message using vsprintf() */
	va_start(args, format);
	vsprintf(message, format, args);
	va_end(args);

	/* copy the error message to a Mercury string */
	restore_registers(); /* for MR_hp */
	make_aligned_string(message_as_mercury_string, message);
	save_registers(); /* for MR_hp */

	/* call some Mercury code to throw the exception */
	ML_throw_io_error((String) message_as_mercury_string);
}

").

:- pragma c_code("

void
mercury_output_error(MercuryFile *mf)
{
	mercury_io_error(mf, ""error writing to output file: %s"",
		strerror(errno));
}

").

:- pragma c_code("

void
mercury_print_string(MercuryFile* mf, const char *s)
{
	if (fprintf(mf->file, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
	while (*s) {
		if (*s++ == '\\n') {
			mf->line_number++;
		}
	}
}

").

:- pragma c_code("

void
mercury_print_binary_string(MercuryFile* mf, const char *s)
{
	if (fprintf(mf->file, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
}

").

:- pragma c_code("

int
mercury_getc(MercuryFile* mf)
{
	int c = getc(mf->file);
	if (c == '\\n') {
		mf->line_number++;
	}
	return c;
}

").

:- pragma c_code("

void
mercury_close(MercuryFile* mf)
{
	if (mf != &mercury_stdin &&
	    mf != &mercury_stdout &&
	    mf != &mercury_stderr)
	{
		if (fclose(mf->file) < 0) {
			mercury_io_error(mf, ""error closing file: %s"",
				strerror(errno));
		}
		oldmem(mf);
	}
}

").

/* input predicates */

:- pragma c_code(io__read_char_code(File::in, CharCode::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	CharCode = mercury_getc((MercuryFile *) File);
	update_io(IO0, IO);
").

:- pragma c_code(io__putback_char(File::in, Character::in, IO0::di, IO::uo),
		may_call_mercury, "{
	MercuryFile* mf = (MercuryFile *) File;
	if (Character == '\\n') {
		mf->line_number--;
	}
	/* XXX should work even if ungetc() fails */
	if (ungetc(Character, mf->file) == EOF) {
		mercury_io_error(mf, ""io__putback_char: ungetc failed"");
	}
	update_io(IO0, IO);
}").

:- pragma c_code(io__putback_byte(File::in, Character::in, IO0::di, IO::uo),
		may_call_mercury, "{
	MercuryFile* mf = (MercuryFile *) File;
	/* XXX should work even if ungetc() fails */
	if (ungetc(Character, mf->file) == EOF) {
		mercury_io_error(mf, ""io__putback_byte: ungetc failed"");
	}
	update_io(IO0, IO);
}").

/* output predicates - with output to mercury_current_text_output */

:- pragma c_code(io__write_string(Message::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	mercury_print_string(mercury_current_text_output, Message);
	update_io(IO0, IO);
").

:- pragma c_code(io__write_char(Character::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	if (putc(Character, mercury_current_text_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	if (Character == '\\n') {
		mercury_current_text_output->line_number++;
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__write_int(Val::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	if (fprintf(mercury_current_text_output->file, ""%ld"", (long) Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__write_float(Val::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	if (fprintf(mercury_current_text_output->file, ""%#.15g"", Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__write_byte(Byte::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	/* call putc with a strictly non-negative byte-sized integer */
	if (putc((int) ((unsigned char) Byte),
			mercury_current_binary_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__write_bytes(Message::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	mercury_print_binary_string(mercury_current_binary_output, Message);
	update_io(IO0, IO);
}").

:- pragma c_code(io__flush_output(IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	if (fflush(mercury_current_text_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__flush_binary_output(IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	if (fflush(mercury_current_binary_output->file) < 0) {
		mercury_output_error(mercury_current_binary_output);
	}
	update_io(IO0, IO);
").

/* moving about binary streams */

:- pred whence_to_int(io__whence::in, int::out) is det.

whence_to_int(set, 0).
whence_to_int(cur, 1).
whence_to_int(end, 2).

io__seek_binary(Stream, Whence, Offset, IO0, IO) :-
	whence_to_int(Whence, Flag),
	io__seek_binary_2(Stream, Flag, Offset, IO0, IO).

:- pred io__seek_binary_2(io__stream, int, int, io__state, io__state).
:- mode io__seek_binary_2(in, in, in, di, uo) is det.

:- pragma c_code(io__seek_binary_2(Stream::in, Flag::in, Off::in,
	IO0::di, IO::uo), [will_not_call_mercury, thread_safe],
"{
	static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };
	MercuryFile *stream = (MercuryFile *) Stream;
	/* XXX should check for failure */
	fseek(stream->file, Off, seek_flags[Flag]);
	IO = IO0;
}").

:- pragma c_code(io__binary_stream_offset(Stream::in, Offset::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	/* XXX should check for failure */
	Offset = ftell(stream->file);
	IO = IO0;
}").


/* output predicates - with output to the specified stream */

:- pragma c_code(io__write_string(Stream::in, Message::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], 
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma c_code(io__write_char(Stream::in, Character::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], 
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (putc(Character, stream->file) < 0) {
		mercury_output_error(stream);
	}
	if (Character == '\\n') {
		stream->line_number++;
	}
	update_io(IO0, IO);
}").

:- pragma c_code(io__write_int(Stream::in, Val::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fprintf(stream->file, ""%ld"", (long) Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma c_code(io__write_float(Stream::in, Val::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fprintf(stream->file, ""%#.15g"", Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma c_code(io__write_byte(Stream::in, Byte::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	MercuryFile *stream = (MercuryFile *) Stream;
	/* call putc with a strictly non-negative byte-sized integer */
	if (putc((int) ((unsigned char) Byte), stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma c_code(io__write_bytes(Stream::in, Message::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_binary_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma c_code(io__flush_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fflush(stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma c_code(io__flush_binary_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fflush(stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

/* stream predicates */

:- pragma export(io__stdin_stream(out, di, uo), "ML_io_stdin_stream").
:- pragma export(io__stdout_stream(out, di, uo), "ML_io_stdout_stream").
:- pragma export(io__stderr_stream(out, di, uo), "ML_io_stderr_stream").

:- pragma c_code(io__stdin_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Stream = (Word) &mercury_stdin;
	update_io(IO0, IO);
").

:- pragma c_code(io__stdout_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Stream = (Word) &mercury_stdout;
	update_io(IO0, IO);
").

:- pragma c_code(io__stderr_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Stream = (Word) &mercury_stderr;
	update_io(IO0, IO);
").

:- pragma c_code(io__stdin_binary_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Stream = (Word) &mercury_stdin_binary;
	update_io(IO0, IO);
").

:- pragma c_code(io__stdout_binary_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Stream = (Word) &mercury_stdout_binary;
	update_io(IO0, IO);
").

:- pragma c_code(io__input_stream(Stream::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	Stream = (Word) mercury_current_text_input;
	update_io(IO0, IO);
").

:- pragma c_code(io__output_stream(Stream::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	Stream = (Word) mercury_current_text_output;
	update_io(IO0, IO);
").

:- pragma c_code(io__binary_input_stream(Stream::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	Stream = (Word) mercury_current_binary_input;
	update_io(IO0, IO);
").

:- pragma c_code(io__binary_output_stream(Stream::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	Stream = (Word) mercury_current_binary_output;
	update_io(IO0, IO);
").

:- pragma c_code(io__get_line_number(LineNum::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	LineNum = mercury_current_text_input->line_number;
	update_io(IO0, IO);
").
	
:- pragma c_code(
	io__get_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
		will_not_call_mercury, "{
	MercuryFile *stream = (MercuryFile *) Stream;
	LineNum = stream->line_number;
	update_io(IO0, IO);
}").
	
:- pragma c_code(io__set_line_number(LineNum::in, IO0::di, IO::uo),
		will_not_call_mercury, "
	mercury_current_text_input->line_number = LineNum;
	update_io(IO0, IO);
").
	
:- pragma c_code(
	io__set_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
		will_not_call_mercury, "{
	MercuryFile *stream = (MercuryFile *) Stream;
	stream->line_number = LineNum;
	update_io(IO0, IO);
}").
	
:- pragma c_code(io__get_output_line_number(LineNum::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	LineNum = mercury_current_text_output->line_number;
	update_io(IO0, IO);
").
	
:- pragma c_code(
	io__get_output_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
		will_not_call_mercury, "{
	MercuryFile *stream = (MercuryFile *) Stream;
	LineNum = stream->line_number;
	update_io(IO0, IO);
}").

:- pragma c_code(io__set_output_line_number(LineNum::in, IO0::di, IO::uo),
		will_not_call_mercury, "
	mercury_current_text_output->line_number = LineNum;
	update_io(IO0, IO);
").
	
:- pragma c_code(
	io__set_output_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
		will_not_call_mercury, "{
	MercuryFile *stream = (MercuryFile *) Stream;
	stream->line_number = LineNum;
	update_io(IO0, IO);
}").
	
% io__set_input_stream(NewStream, OldStream, IO0, IO1)
%	Changes the current input stream to the stream specified.
%	Returns the previous stream.
:- pragma c_code(
	io__set_input_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	OutStream = (Word) mercury_current_text_input;
	mercury_current_text_input = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma c_code(
	io__set_output_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	OutStream = (Word) mercury_current_text_output;
	mercury_current_text_output = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma c_code(
	io__set_binary_input_stream(NewStream::in, OutStream::out,
			IO0::di, IO::uo), will_not_call_mercury, "
	OutStream = (Word) mercury_current_binary_input;
	mercury_current_binary_input = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma c_code(
	io__set_binary_output_stream(NewStream::in, OutStream::out,
			IO0::di, IO::uo), will_not_call_mercury, "
	OutStream = (Word) mercury_current_binary_output;
	mercury_current_binary_output = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

/* stream open/close predicates */

% io__do_open(File, Mode, ResultCode, Stream, IO0, IO1).
%	Attempts to open a file in the specified mode.
%	ResultCode is 0 for success, -1 for failure.
:- pragma c_code(
	io__do_open(FileName::in, Mode::in, ResultCode::out,
			Stream::out, IO0::di, IO::uo),
			[will_not_call_mercury, thread_safe],
"
	Stream = (Word) mercury_open(FileName, Mode);
	ResultCode = (Stream ? 0 : -1);
	update_io(IO0, IO);
").

:- pragma c_code(io__close_input(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	mercury_close((MercuryFile *) Stream);
	update_io(IO0, IO);
").

:- pragma c_code(io__close_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	mercury_close((MercuryFile *) Stream);
	update_io(IO0, IO);
").

:- pragma c_code(io__close_binary_input(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	mercury_close((MercuryFile *) Stream);
	update_io(IO0, IO);
").

:- pragma c_code(io__close_binary_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, thread_safe], "
	mercury_close((MercuryFile *) Stream);
	update_io(IO0, IO);
").

/* miscellaneous predicates */

:- pragma c_code(
	io__progname(DefaultProgname::in, PrognameOut::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	if (progname) {
		/* The silly casting below is needed to avoid
		   a gcc warning about casting away const.
		   PrognameOut is of type `String' (char *);
		   it should be of type `ConstString' (const char *),
		   but fixing that requires a fair bit of work
		   on the compiler.  */
		make_aligned_string(LVALUE_CAST(ConstString, PrognameOut),
			progname);
	} else {
		PrognameOut = DefaultProgname;
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__command_line_arguments(Args::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	/* convert mercury_argv from a vector to a list */
	{ int i = mercury_argc;
	  Args = MR_list_empty_msg(mercury__io__command_line_arguments_3_0);
	  while (--i >= 0) {
		Args = MR_list_cons_msg((Word) mercury_argv[i], Args,
			mercury__io__command_line_arguments_3_0);
	  }
	}
	update_io(IO0, IO);
").

:- pragma c_code(io__get_exit_status(ExitStatus::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	ExitStatus = mercury_exit_status;
	update_io(IO0, IO);
").

:- pragma c_code(io__set_exit_status(ExitStatus::in, IO0::di, IO::uo),
		will_not_call_mercury, "
	mercury_exit_status = ExitStatus;
	update_io(IO0, IO);
").

:- pragma c_code(
	io__call_system_code(Command::in, Status::out, IO0::di, IO::uo),
		will_not_call_mercury, "
	Status = system(Command);
	if ( Status == -1 || Status == 127 ) {
		/* 
		** Return values of 127 or -1 from system() indicate that
		** the system call failed.  Dont return -1, as -1 indicates
		** that the system call was killed by signal number 1. 
		*/
		Status = 127;
	} else {
		#if defined (WIFEXITED) && defined (WEXITSTATUS) && \
			defined (WIFSIGNALED) && defined (WTERMSIG)
		if (WIFEXITED(Status))
			Status = WEXITSTATUS(Status);
		else if (WIFSIGNALED(Status))
			Status = -WTERMSIG(Status);
		else
			Status = 127;
	
		#else
		if (Status & 0xff != 0) 
			/* the process was killed by a signal */
			Status = -(Status & 0xff);
		else 
			/* the process terminated normally */
			Status = (Status & 0xff00) >> 8;
	
		#endif
	}
	update_io(IO0, IO);
").

/*---------------------------------------------------------------------------*/

/* io__getenv and io__putenv, from io.m */

:- pragma c_code(io__getenv(Var::in, Value::out), will_not_call_mercury, "{
	Value = getenv(Var);
	SUCCESS_INDICATOR = (Value != 0);
}").

:- pragma c_code(io__putenv(VarAndValue::in), will_not_call_mercury, "
	SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);
").

/*---------------------------------------------------------------------------*/

io__tmpnam(Name) -->
	io__make_temp(Name),
	io__remove_file(Name, _Result).

io__tmpnam(Dir, Prefix, Name) -->
	io__make_temp(Dir, Prefix, Name),
	io__remove_file(Name, _Result).

/*---------------------------------------------------------------------------*/

	% We need to do an explicit check of TMPDIR because not all
	% systems check TMPDIR for us (eg Linux #$%*@&).
io__make_temp(Name) -->
	io__get_environment_var("TMPDIR", Result),
	(
		{ Result = yes(Dir) }
	;
		{ Result = no },
		{ Dir = "/tmp" }
	),
	io__make_temp(Dir, "mtmp", Name).

/*---------------------------------------------------------------------------*/

/*
** XXX	The code for io__make_temp assumes POSIX.
**	It uses the functions open(), close(), and getpid()
**	and the macros EEXIST, O_WRONLY, O_CREAT, and O_EXCL.
**	We should be using conditional compilation here to
**	avoid these POSIX dependencies.
*/

%#include <stdio.h>

:- pragma c_header_code("
	#include <unistd.h>
	#include <sys/types.h>
	#include <sys/stat.h>
	#include <fcntl.h>

	#define	MAX_TEMPNAME_TRIES	(6 * 4)

	extern long ML_io_tempnam_counter;
").

:- pragma c_code("
	long	ML_io_tempnam_counter = 0;
").

:- pragma c_code(io__make_temp(Dir::in, Prefix::in, FileName::out,
		IO0::di, IO::uo),
		[may_call_mercury, thread_safe],
"{
	/*
	** Constructs a temporary name by concatenating Dir, `/',
	** the first 5 chars of Prefix, three hex digits, '.',
	** and 3 more hex digits.  The six digit hex number is generated
	** by starting with the pid of this process.
	** Uses `open(..., O_CREATE | O_EXCL, ...)' to create the file,
	** checking that there was no existing file with that name.
	*/
	int	len, err, fd, num_tries;
	char	countstr[256];

	len = strlen(Dir) + 1 + 5 + 3 + 1 + 3 + 1;
		/* Dir + / + Prefix + counter_high + . + counter_low + \\0 */
	incr_hp_atomic_msg(LVALUE_CAST(Word, FileName),
		(len + sizeof(Word)) / sizeof(Word),
		mercury__io__make_temp_5_0, ""string:string/0"");
	if (ML_io_tempnam_counter == 0) {
		ML_io_tempnam_counter = getpid();
	}
	num_tries = 0;
	do {
		sprintf(countstr, ""%06lX"", ML_io_tempnam_counter & 0xffffffL);
		strcpy(FileName, Dir);
		strcat(FileName, ""/"");
		strncat(FileName, Prefix, 5);
		strncat(FileName, countstr, 3);
		strcat(FileName, ""."");
		strncat(FileName, countstr + 3, 3);
		fd = open(FileName, O_WRONLY | O_CREAT | O_EXCL, 0600);
		num_tries++;
		ML_io_tempnam_counter += (1 << num_tries);
	} while (fd == -1 && errno == EEXIST &&
		num_tries < MAX_TEMPNAME_TRIES);
	if (fd == -1) {
		mercury_io_error(NULL,
			""error opening temporary file `%s': %s"",
			FileName, strerror(errno));
	} 
	err = close(fd);
	if (err != 0) {
		mercury_io_error(NULL,
			""error closing temporary file `%s': %s"",
			FileName, strerror(errno));
	}
	update_io(IO0, IO);
}").

/*---------------------------------------------------------------------------*/

:- pragma c_header_code("

#include <string.h>
#include <errno.h>

/*
** ML_maybe_make_err_msg(was_error, msg, error_msg):
**	if `was_error' is true, then append `msg' and `strerror(errno)'
**	to give `error_msg'; otherwise, set `error_msg' to NULL.
**
** This is defined as a macro rather than a C function
** to avoid worrying about the `hp' register being
** invalidated by the function call.
** It also needs to be a macro because incr_hp_atomic_msg()
** stringizes its third argument.
*/
#define ML_maybe_make_err_msg(was_error, msg, procname, error_msg)	\\
	do {								\\
		char *errno_msg;					\\
		size_t len;						\\
		Word tmp;						\\
									\\
		if (was_error) {					\\
			errno_msg = strerror(errno);			\\
			len = strlen(msg) + strlen(errno_msg);		\\
			incr_hp_atomic_msg(tmp,				\\
				(len + sizeof(Word)) / sizeof(Word),	\\
				procname,				\\
				""string:string/0"");			\\
			(error_msg) = (char *)tmp;			\\
			strcpy((error_msg), msg);			\\
			strcat((error_msg), errno_msg);			\\
		} else {						\\
			(error_msg) = NULL;				\\
		}							\\
	} while(0)

").

io__remove_file(FileName, Result, IO0, IO) :-
	io__remove_file_2(FileName, Res, ResString, IO0, IO),
	( Res \= 0 ->
		Result = error(io_error(ResString))
	;
		Result = ok
	).

:- pred io__remove_file_2(string, int, string, io__state, io__state).
:- mode io__remove_file_2(in, out, out, di, uo) is det.

:- pragma c_code(io__remove_file_2(FileName::in, RetVal::out, RetStr::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe],
"{
	RetVal = remove(FileName);
	ML_maybe_make_err_msg(RetVal != 0, ""remove failed: "",
		mercury__io__remove_file_2_5_0, RetStr);
	update_io(IO0, IO);
}").

io__rename_file(OldFileName, NewFileName, Result, IO0, IO) :-
	io__rename_file_2(OldFileName, NewFileName, Res, ResString, IO0, IO),
	( Res \= 0 ->
		Result = error(io_error(ResString))
	;
		Result = ok
	).

:- pred io__rename_file_2(string, string, int, string, io__state, io__state).
:- mode io__rename_file_2(in, in, out, out, di, uo) is det.

:- pragma c_code(io__rename_file_2(OldFileName::in, NewFileName::in,
		RetVal::out, RetStr::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe],
"{
	RetVal = rename(OldFileName, NewFileName);
	ML_maybe_make_err_msg(RetVal != 0, ""rename failed: "",
		mercury__io__rename_file_2_6_0, RetStr);
	update_io(IO0, IO);
}").

/*---------------------------------------------------------------------------*/

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%	Functional forms added.

:- interface.

:- func io__error_message(io__error) = string.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

io__error_message(Error) = Msg :-
	io__error_message(Error, Msg).

