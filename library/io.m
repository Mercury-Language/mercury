%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2002 The University of Melbourne.
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
% Attempting any operation on a stream which has already been closed results
% in undefined behaviour.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.
:- import_module char, string, std_util, list, time, deconstruct.

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

	% An alternative, more concise name for `io__state'.

:- type io__io == io__state.

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

	% io__maybe_partial_res is used where it is possible to return
	% a partial result when an error occurs,
:- type io__maybe_partial_res(T)
			--->	ok(T)
			;	error(T, io__error).

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

:- pred io__read_file(io__maybe_partial_res(list(char)), io__state, io__state).
:- mode io__read_file(out, di, uo) is det.
%		Reads all the characters from the current input stream until
%		eof or error.

:- pred io__read_file_as_string(io__maybe_partial_res(string),
			io__state, io__state).
:- mode io__read_file_as_string(out, di, uo) is det.
%		Reads all the characters from the current input stream until
%		eof or error.  Returns the result as a string rather than
%		as a list of char.

:- pred io__input_stream_foldl(pred(char, T, T),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__input_stream_foldl((pred(in, in, out) is det),
			in, out, di, uo) is det.
:- mode io__input_stream_foldl((pred(in, in, out) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error.

:- pred io__input_stream_foldl_io(pred(char, io__state, io__state),
			io__res, io__state, io__state).
:- mode io__input_stream_foldl_io((pred(in, di, uo) is det),
			out, di, uo) is det.
:- mode io__input_stream_foldl_io((pred(in, di, uo) is cc_multi),
			out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error.

:- pred io__input_stream_foldl2_io(pred(char, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__input_stream_foldl2_io((pred(in, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__input_stream_foldl2_io((pred(in, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error.

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

:- pred io__read_file(io__input_stream, io__maybe_partial_res(list(char)),
			io__state, io__state).
:- mode io__read_file(in, out, di, uo) is det.
%		Reads all the characters from the given input stream until
%		eof or error.

:- pred io__read_file_as_string(io__input_stream,
			io__maybe_partial_res(string), io__state, io__state).
:- mode io__read_file_as_string(in, out, di, uo) is det.
%		Reads all the characters from the given input stream until
%		eof or error.  Returns the result as a string rather than
%		as a list of char.

:- pred io__input_stream_foldl(io__input_stream, pred(char, T, T),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__input_stream_foldl(in, (pred(in, in, out) is det),
			in, out, di, uo) is det.
:- mode io__input_stream_foldl(in, (pred(in, in, out) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error.

:- pred io__input_stream_foldl_io(io__input_stream,
			pred(char, io__state, io__state), io__res,
			io__state, io__state).
:- mode io__input_stream_foldl_io(in, (pred(in, di, uo) is det),
			out, di, uo) is det.
:- mode io__input_stream_foldl_io(in, (pred(in, di, uo) is cc_multi),
			out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error.

:- pred io__input_stream_foldl2_io(io__input_stream,
			pred(char, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__input_stream_foldl2_io(in, (pred(in, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__input_stream_foldl2_io(in, (pred(in, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error.

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
%
%		First, the input stream is read until an end-of-term token,
%		end-of-file, or I/O error is reached.  (An end-of-term
%		token consists of a `.' followed by whitespace.
%		The trailing whitespace is left in the input stream.)
%
%		Then, the result is determined according to the tokens read.
%		If there were no non-whitespace characters before the
%		end of file, then `io__read' returns `eof'.
%		If the tokens read formed a syntactically correct ground term
%		of the correct type, followed by an end-of-term token,
%		then it returns `ok(Term)'.  If characters read from
%		the input stream did not form a syntactically
%		correct term, or if the term read is not a ground term,
%		or if the term is not a valid term of the appropriate type,
%		or if an I/O error is encountered, then it returns
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

:- pred io__print(io__output_stream, deconstruct__noncanon_handling, T,
	io__state, io__state).
:- mode io__print(in, in(do_not_allow), in, di, uo) is det.
:- mode io__print(in, in(canonicalize), in, di, uo) is det.
:- mode io__print(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__print(in, in, in, di, uo) is cc_multi.

:- pred io__print_cc(T, io__state, io__state).
:- mode io__print_cc(in, di, uo) is cc_multi.

%	io__print/3 writes its argument to the standard output stream.
%	io__print/4 writes its second argument to the output stream
%	specified in its first argument.
%	In all cases, the argument to output can be of any type.
%	It is output in a format that is intended to be human readable.
%
%	If the argument is just a single string or character, it
%	will be printed out exactly as is (unquoted).
%	If the argument is of type univ, then it will print out
%	the value stored in the univ, but not the type.
%
%	io__print/5 is the same as io__print/4 except that it allows
%	the caller to specify how non-canonical types should be handled.
%	io__print/3 and io__print/4 implicitly specify `canonicalize'
%	as the method for handling non-canonical types.  This means
%	that for higher-order types, or types with user-defined
%	equality axioms, or types defined using the foreign language
%	interface (i.e. c_pointer type or pragma foreign_type),
%	the text output will only describe the type that is being
%	printed, not the value.
%
%	io__print_cc/3 is the same as io__print/3 except that it
%	specifies `include_details_cc' rather than `canonicalize'.
%	This means that it will print the details of non-canonical
%	types.  However, it has determinism `cc_multi'.
%
%	Note that even if `include_details_cc' is specified,
%	some implementations may not be able to print all the details
%	for higher-order types or types defined using the foreign
%	language interface.

:- pred io__write(T, io__state, io__state).
:- mode io__write(in, di, uo) is det.

:- pred io__write(io__output_stream, T, io__state, io__state).
:- mode io__write(in, in, di, uo) is det.

:- pred io__write(io__output_stream, deconstruct__noncanon_handling, T,
	io__state, io__state).
:- mode io__write(in, in(do_not_allow), in, di, uo) is det.
:- mode io__write(in, in(canonicalize), in, di, uo) is det.
:- mode io__write(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__write(in, in, in, di, uo) is cc_multi.

:- pred io__write_cc(T, io__state, io__state).
:- mode io__write_cc(in, di, uo) is cc_multi.

%	io__write/3 writes its argument to the current output stream.
%	io__write/4 writes its second argument to the output stream
%	specified in its first argument.
%	In all cases, the argument to output may be of any type.
%	The argument is written in a format that is intended to
%	be valid Mercury syntax whenever possible.
%
%	Strings and characters are always printed out in quotes,
%	using backslash escapes if necessary.
%	For higher-order types, or for types defined using the
%	foreign language interface (pragma foreign_code), the text
%	output will only describe the type that is being printed, not
%	the value, and the result may not be parsable by `io__read'.
%	For the types containing existential quantifiers,
%	the type `type_desc' and closure types, the result may not be
%	parsable by `io__read', either.  But in all other cases the
%	format used is standard Mercury syntax, and if you append a
%	period and newline (".\n"), then the results can be read in
%	again using `io__read'.
%
%	io__write/5 is the same as io__write/4 except that it allows
%	the caller to specify how non-canonical types should be handled.
%	io__write_cc/3 is the same as io__write/3 except that it
%	specifies `include_details_cc' rather than `canonicalize'.

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
:- mode io__write_list(in, in, pred(in, di, uo) is cc_multi, di, uo)
	is cc_multi.
	% io__write_list(List, Separator, OutputPred, IO0, IO)
	% applies OutputPred to each element of List, printing Separator
	% between each element. Outputs to the current output stream.

:- pred io__write_list(io__output_stream, list(T), string, 
	pred(T, io__state, io__state), io__state, io__state).
:- mode io__write_list(in, in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode io__write_list(in, in, in, pred(in, di, uo) is cc_multi, di, uo)
	is cc_multi.
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
%		This will throw an io__error exception
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
%		This will throw an io__error exception
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

:- pred io__current_input_stream(io__input_stream, io__state, io__state).
:- mode io__current_input_stream(out, di, uo) is det.
%       io__current_input_stream(CurrentStream, IO0, IO1)
%		Returns the current input stream in CurrentStream.
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
%		This will throw an io__error exception
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
%		This will throw an io__error exception
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

:- pred io__current_output_stream(io__output_stream, io__state, io__state).
:- mode io__current_output_stream(out, di, uo) is det.
%       io__current_output_stream(CurrentStream, IO0, IO1)
%		Returns the current output stream in CurrentStream.
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
%		This will throw an io__error exception
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
%		This will throw an io__error exception
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

:- pred io__current_binary_input_stream(io__binary_input_stream,
			io__state, io__state).
:- mode io__current_binary_input_stream(out, di, uo) is det.
%       io__current_binary_input_stream(CurrentStream, IO0, IO1)
%		Returns the current binary input stream in CurrentStream.
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
%		This will throw an io__error exception
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
%		This will throw an io__error exception
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

:- pred io__current_binary_output_stream(io__binary_output_stream,
			io__state, io__state).
:- mode io__current_binary_output_stream(out, di, uo) is det.
%       io__current_binary_output_stream(CurrentStream, IO0, IO1)
%		Returns the current binary output stream in CurrentStream.
%		Does not modify the IO state.

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

:- pred io__file_modification_time(string, io__res(time_t),
		io__state, io__state).
:- mode io__file_modification_time(in, out, di, uo) is det.
	% io__file_modification_time(FileName, TimeResult)
	% finds the last modification time of the given file.
	% This predicate will only work on systems which provide
	% the POSIX C library function stat(). On other systems the
	% returned result will always be bound to error/1.

%-----------------------------------------------------------------------------%

% Memory management predicates.

	% Write memory/time usage statistics to stderr.

:- pred io__report_stats(io__state, io__state).
:- mode io__report_stats(di, uo) is det.

	% Write complete memory usage statistics to stderr,
	% including information about all procedures and types.
	% (You need to compile with memory profiling enabled.)
	%
	% OBSOLETE: call io__report_stats/3 instead, with the first
	% specified as "full_memory_stats".

:- pragma obsolete(io__report_full_memory_stats/2).
:- pred io__report_full_memory_stats(io__state, io__state).
:- mode io__report_full_memory_stats(di, uo) is det.

	% Write statistics to stderr; what statistics will be written
	% is controlled by the first argument, which acts a selector.
	% What selector values cause what statistics to be printed
	% is implementation defined.
	%
	% The Melbourne implementation supports the following selectors:
	%
	% "standard"		Writes memory/time usage statistics.
	%
	% "full_memory_stats"	Writes complete memory usage statistics,
	%			including information about all procedures
	%			and types. Requires compilation with
	%			memory profiling enabled.
	%
	% "tabling"		Writes statistics about the internals
	%			of the tabling system. Requires the runtime
	%			to have been compiled with the macro
	%			MR_TABLE_STATISTICS defined.

:- pred io__report_stats(string, io__state, io__state).
:- mode io__report_stats(in, di, uo) is det.

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

:- type io__system_result
	--->	exited(int)
	;	signalled(int)
	.

:- pred io__call_system_return_signal(string, io__res(io__system_result),
		io__state, io__state).
:- mode io__call_system_return_signal(in, out, di, uo) is det.
%	io__call_system_return_signal(Command, Result, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Result is either `ok(ExitStatus)' if it was
%		possible to invoke the command and the it ran to completion,
%		`signal(SignalNum)' if the command was killed by a signal, or
%		`error(ErrorCode)' if the command could not be executed.
%		The `ExitStatus' will be 0 if the command completed
%		successfully or the return value of the command otherwise.

:- func io__error_message(io__error) = string.
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

% For use by term_io.m:

:- import_module ops.

:- pred io__get_op_table(ops__table, io__state, io__state).
:- mode io__get_op_table(out, di, uo) is det.

:- pred io__set_op_table(ops__table, io__state, io__state).
:- mode io__set_op_table(di, di, uo) is det.

% For use by browser/browse.m:

:- pred io__write_univ(univ, io__state, io__state).
:- mode io__write_univ(in, di, uo) is det.

:- pred io__write_univ(io__output_stream, univ, io__state, io__state).
:- mode io__write_univ(in, in, di, uo) is det.

:- pred io__write_univ(io__output_stream, deconstruct__noncanon_handling, univ,
	io__state, io__state).
:- mode io__write_univ(in, in(do_not_allow), in, di, uo) is det.
:- mode io__write_univ(in, in(canonicalize), in, di, uo) is det.
:- mode io__write_univ(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__write_univ(in, in, in, di, uo) is cc_multi.

% For use by extras/aditi/aditi.m

	% This is the same as io__read_from_string, except that an integer
	% is allowed where a character is expected. This is needed because
	% Aditi does not have a builtin character type. This also allows an
	% integer where a float is expected.
:- pred io__read_from_string_with_int_instead_of_char(string, string, int,
			io__read_result(T), posn, posn).
:- mode io__read_from_string_with_int_instead_of_char(in, in, in,
			out, in, out) is det.

% For use by compiler/process_util.m:

	% Interpret the child process exit status returned by
	% system() or wait().
:- func io__handle_system_command_exit_status(int) =
		io__res(io__system_result).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, dir, term, term_io, varset, require, benchmarking, array.
:- import_module bool, int, parser, exception.
:- use_module table_builtin.
:- use_module rtti_implementation.

:- type io__state ---> io__state(c_pointer).
	% Values of type `io__state' are never really used:
	% instead we store data in global variables.
	% The reason this is not defined simply as `io__state == c_pointer'
	% is so that `type_name' produces more informative results
	% for cases such as `type_name(main)'.

:- pragma foreign_decl("C", "
	extern MR_Word		ML_io_stream_names;
	extern MR_Word		ML_io_user_globals;
	#if 0
	  extern MR_Word	ML_io_ops_table;
	#endif
").

:- pragma foreign_code("C", "
	MR_Word			ML_io_stream_names;
	MR_Word			ML_io_user_globals;
	#if 0
	  MR_Word		ML_io_ops_table;
	#endif
").

:- pragma foreign_code("MC++", "
#ifdef MR_HIGHLEVEL_DATA
	static mercury::tree234::tree234_2 __gc	*ML_io_stream_names;
#else
	static MR_Word		ML_io_stream_names;
#endif
	static MR_Univ		ML_io_user_globals;
	static int next_id;
	static System::Text::ASCIIEncoding *ascii_encoder;
").


:- type io__stream_names ==	map(io__stream_id, string).
:- type io__stream_putback ==	map(io__stream_id, list(char)).

:- type io__input_stream ==	io__stream.
:- type io__output_stream ==	io__stream.

:- type io__binary_stream ==	io__stream.

:- type io__stream == c_pointer.

	% a unique identifier for an IO stream
:- type io__stream_id == int.

:- func io__get_stream_id(io__stream) = io__stream_id.

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

:- pred io__call_system_code(string, int, string, io__state, io__state).
:- mode io__call_system_code(in, out, out, di, uo) is det.
%	io__call_system_code(Command, Status, Message, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Returns Status = 127 and Message on failure.
%		Otherwise returns the raw exit status from the system()
%		call.

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

% we want to inline these, to allow deforestation
:- pragma inline(io__read_char/3).
:- pragma inline(io__read_char/4).

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

% we want to inline these, to allow deforestation
:- pragma inline(io__read_byte/3).
:- pragma inline(io__read_byte/4).

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

:- pragma foreign_proc("C", io__read_line_as_string_2(File::in, Res :: out,
			RetString::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure,
			tabled_for_io, thread_safe],
"
#define ML_IO_READ_LINE_GROW(n)	((n) * 3 / 2)
#define ML_IO_BYTES_TO_WORDS(n)	(((n) + sizeof(MR_Word) - 1) / sizeof(MR_Word))
#define ML_IO_READ_LINE_START	1024

	MR_Char initial_read_buffer[ML_IO_READ_LINE_START];
	MR_Char *read_buffer = initial_read_buffer;
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
		if (char_code != (MR_Char) char_code) {
			Res = -2;
			break;
		}
		read_buffer[i++] = char_code;
		MR_assert(i <= read_buf_size);
		if (i == read_buf_size) {
			/* Grow the read buffer */
			read_buf_size = ML_IO_READ_LINE_GROW(read_buf_size);
			if (read_buffer == initial_read_buffer) {
				read_buffer = MR_NEW_ARRAY(MR_Char,
						read_buf_size);
				memcpy(read_buffer, initial_read_buffer,
					ML_IO_READ_LINE_START);
			} else {
				read_buffer = MR_RESIZE_ARRAY(read_buffer,
						MR_Char, read_buf_size);
			}
		}
	}
	if (Res == 0) {
		MR_incr_hp_atomic_msg(MR_LVALUE_CAST(MR_Word, RetString),
			ML_IO_BYTES_TO_WORDS((i + 1) * sizeof(MR_Char)),
			MR_PROC_LABEL, ""string:string/0"");
		memcpy(RetString, read_buffer, i * sizeof(MR_Char));
		RetString[i] = '\\0';
	} else {
		RetString = NULL;
	}
	if (read_buffer != initial_read_buffer) {
		MR_free(read_buffer);
	}
	update_io(IO0, IO);
").

io__read_line_as_string_2(_, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__read_line_as_string_2") }.

io__read_file(Result) -->
	io__input_stream(Stream),
	io__read_file(Stream, Result).

io__read_file(Stream, Result) -->
	io__read_file_2(Stream, [], Result).

:- pred io__read_file_2(io__input_stream, list(char),
		io__maybe_partial_res(list(char)), io__state, io__state).
:- mode io__read_file_2(in, in, out, di, uo) is det.

io__read_file_2(Stream, Chars0, Result) -->
	io__read_char(Stream, Result0),
	(
		{ Result0 = eof },
		{ Result = ok(list__reverse(Chars0)) }
	;
		{ Result0 = error(Err) },
		{ Result = error(list__reverse(Chars0), Err) }
	;
		{ Result0 = ok(Char) },
		io__read_file_2(Stream, [Char|Chars0], Result)
	).

%-----------------------------------------------------------------------------%

io__read_file_as_string(Result) -->
	io__input_stream(Stream),
	io__read_file_as_string(Stream, Result).

io__read_file_as_string(Stream, Result) -->
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
	io__check_err(Stream, Result0),
	{
		Result0 = ok,
		Result = ok(String)
	;
		Result0 = error(Error),
		Result = error(String, Error)
	}.

:- pred io__read_file_as_string_2(io__input_stream, buffer, int, int,
		buffer, int, int, io__state, io__state).
:- mode io__read_file_as_string_2(in, di, in, in,
		uo, out, out, di, uo) is det.

io__read_file_as_string_2(Stream, Buffer0, Pos0, Size0, Buffer, Pos, Size) -->
	io__read_into_buffer(Stream, Buffer0, Pos0, Size0,
		Buffer1, Pos1),
	( { Pos1 =< Pos0 } ->
		% end-of-file or error
		{ Size = Size0 },
		{ Pos = Pos0 },
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

io__input_stream_foldl(Pred, T0, Res) -->
	io__input_stream(Stream),
	io__input_stream_foldl(Stream, Pred, T0, Res).

io__input_stream_foldl(Stream, Pred, T0, Res) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = ok(Char) },
		{ Pred(Char, T0, T1) },
		io__input_stream_foldl(Stream, Pred, T1, Res)
	;
		{ CharResult = eof },
		{ Res = ok(T0) }
	;
		{ CharResult = error(Error) },
		{ Res = error(T0, Error) }
	).

io__input_stream_foldl_io(Pred, Res) -->
	io__input_stream(Stream),
	io__input_stream_foldl_io(Stream, Pred, Res).

io__input_stream_foldl_io(Stream, Pred, Res) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = ok(Char) },
		Pred(Char),
		io__input_stream_foldl_io(Stream, Pred, Res)
	;
		{ CharResult = eof },
		{ Res = ok }
	;
		{ CharResult = error(Error) },
		{ Res = error(Error) }
	).

io__input_stream_foldl2_io(Pred, T0, Res) -->
	io__input_stream(Stream),
	io__input_stream_foldl2_io(Stream, Pred, T0, Res).

io__input_stream_foldl2_io(Stream, Pred, T0, Res) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = ok(Char) },
		Pred(Char, T0, T1),
		io__input_stream_foldl2_io(Stream, Pred, T1, Res)
	;
		{ CharResult = eof },
		{ Res = ok(T0) }
	;
		{ CharResult = error(Error) },
		{ Res = error(T0, Error) }
	).

%-----------------------------------------------------------------------------%

:- pred io__clear_err(stream, io__state, io__state).
:- mode io__clear_err(in, di, uo) is det.
% same as ANSI C's clearerr().

:- pragma foreign_proc("C", io__clear_err(Stream::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure,
			tabled_for_io, thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;

	if (MR_IS_FILE_STREAM(*f)) {
		clearerr(MR_file(*f));
	} else {
		/* Not a file stream so do nothing */
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++", io__clear_err(_Stream::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe],
"{
	// XXX no error flag to reset as in MC++ an error throws directly an
	// exception (we should create an error indicator in MF_Mercury_file
	// for compatibility)
	update_io(IO0, IO);
}").

io__clear_err(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__clear_err") }.


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

:- pragma foreign_proc("C", ferror(Stream::in, RetVal::out, RetStr::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure,
			tabled_for_io, thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;

	if (MR_IS_FILE_STREAM(*f)) {
		RetVal = ferror(MR_file(*f));
	} else {
		RetVal = -1;
	}

	ML_maybe_make_err_msg(RetVal != 0, ""read failed: "",
		MR_PROC_LABEL, RetStr);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++", ferror(_Stream::in, RetVal::out, _RetStr::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe],
"{
	// XXX see clearerr
	RetVal = 0;
	update_io(IO0, IO);
}").

ferror(_, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("ferror") }.

% io__make_err_msg(MessagePrefix, Message):
%	`Message' is an error message obtained by looking up the
%	message for the current value of errno and prepending
%	`MessagePrefix'.
:- pred io__make_err_msg(string, string, io__state, io__state).
:- mode io__make_err_msg(in, out, di, uo) is det.

:- pragma foreign_proc("C",
	make_err_msg(Msg0::in, Msg::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	ML_maybe_make_err_msg(MR_TRUE, Msg0, MR_PROC_LABEL, Msg);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++", 
	make_err_msg(Msg0::in, Msg::out, _IO0::di, _IO::uo),
		[will_not_call_mercury, promise_pure],
"{
	Msg = System::String::Concat(Msg0, MR_io_exception->Message);
}").

make_err_msg(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__make_err_msg") }.


%-----------------------------------------------------------------------------%

:- pred io__stream_file_size(stream, int, io__state, io__state).
:- mode io__stream_file_size(in, out, di, uo) is det.
% io__stream_file_size(Stream, Size):
%	if Stream is a regular file, then Size is its size (in bytes),
%	otherwise Size is -1.

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_UNISTD_H
	#include <unistd.h>
#endif
#ifdef MR_HAVE_SYS_STAT_H
	#include <sys/stat.h>
#endif
").

:- pragma foreign_proc("C", io__stream_file_size(Stream::in, Size::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;
#if defined(MR_HAVE_FSTAT) && \
    (defined(MR_HAVE_FILENO) || defined(fileno)) && \
    defined(S_ISREG)
	struct stat s;
	if (MR_IS_FILE_STREAM(*f)) {
		if (fstat(fileno(MR_file(*f)), &s) == 0 &&
				S_ISREG(s.st_mode))
		{
			Size = s.st_size;
		} else {
			Size = -1;
		}
	} else {
		Size = -1;
	}
#else
	Size = -1;
#endif
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++", io__stream_file_size(Stream::in, Size::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe],
"{
	MR_MercuryFile mf = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	if (mf->stream->get_CanSeek()) {
		Size = mf->stream->get_Length();
	} else {
	       Size = -1;
	}
	update_io(IO0, IO);
}").

io__stream_file_size(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__stream_file_size") }.
	

io__file_modification_time(File, Result) -->
	io__file_modification_time_2(File, Status, Msg, Time),
	{ Status = 1 ->
		Result = ok(Time)
	;
		Result = error(io_error(Msg))
	}.

:- pred io__file_modification_time_2(string, int, string, time_t,
		io__state, io__state).
:- mode io__file_modification_time_2(in, out, out, out, di, uo) is det.

:- pragma foreign_proc("C", io__file_modification_time_2(FileName::in,
	Status::out, Msg::out, Time::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_STAT
	struct stat s;
	if (stat(FileName, &s) == 0) {
		/* XXX This assumes that a time_t will fit into an MR_Word. */
		Time = s.st_mtime;
		Msg = MR_string_const("""", 0);
		Status = 1;
	} else {
		ML_maybe_make_err_msg(MR_TRUE, ""stat() failed: "",
			MR_PROC_LABEL, Msg);
		Status = 0;
	}
#else
	Status = 0;
	Msg = MR_make_string_const(
	""io__file_modification_time not available on this platform"");
#endif
	update_io(IO0, IO);

}").

io__file_modification_time_2(_, _, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__file_modification_time_2") }.

%-----------------------------------------------------------------------------%

% A `buffer' is just an array of Chars.
% Buffer sizes are measured in Chars.

:- type buffer ---> buffer(c_pointer).

:- pred io__alloc_buffer(int::in, buffer::uo) is det.
:- pragma foreign_proc("C", 
	io__alloc_buffer(Size::in, Buffer::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MR_incr_hp_atomic_msg(Buffer,
		(Size * sizeof(MR_Char) + sizeof(MR_Word) - 1)
			/ sizeof(MR_Word),
		MR_PROC_LABEL, ""io:buffer/0"");
}").

io__alloc_buffer(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("io__alloc_buffer").

:- pred io__resize_buffer(buffer::di, int::in, int::in, buffer::uo) is det.
:- pragma foreign_proc("C",
	io__resize_buffer(Buffer0::di, OldSize::in,
		NewSize::in, Buffer::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MR_Char *buffer0 = (MR_Char *) Buffer0;
	MR_Char *buffer;

#ifdef MR_CONSERVATIVE_GC
	buffer = GC_REALLOC(buffer0, NewSize * sizeof(MR_Char));
#else
	if (buffer0 + OldSize == (MR_Char *) MR_hp) {
		MR_Word next;
		MR_incr_hp_atomic_msg(next, 
		   (NewSize * sizeof(MR_Char) + sizeof(MR_Word) - 1)
		   	/ sizeof(MR_Word),
		   MR_PROC_LABEL,
		   ""io:buffer/0"");
		assert(buffer0 + OldSize == (MR_Char *) next);
	    	buffer = buffer0;
	} else {
		/* just have to alloc and copy */
		MR_incr_hp_atomic_msg(Buffer,
		   (NewSize * sizeof(MR_Char) + sizeof(MR_Word) - 1)
		   	/ sizeof(MR_Word),
		   MR_PROC_LABEL, ""io:buffer/0"");
		buffer = (MR_Char *) Buffer;
		if (OldSize > NewSize) {
			memcpy(buffer, buffer0, NewSize);
		} else {
			memcpy(buffer, buffer0, OldSize);
		}
	}
#endif

	Buffer = (MR_Word) buffer;
}").

io__resize_buffer(_, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("io__resize_buffer").

:- pred io__buffer_to_string(buffer::di, int::in, string::uo) is det.
:- pragma foreign_proc("C", 
	io__buffer_to_string(Buffer::di, Len::in, Str::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	Str = (MR_String) Buffer;
	Str[Len] = '\\0';
}").

io__buffer_to_string(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("io__buffer_to_string/3").

:- pred io__buffer_to_string(buffer::di, string::uo) is det.
:- pragma foreign_proc("C",
	io__buffer_to_string(Buffer::di, Str::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	Str = (MR_String) Buffer;
}").

io__buffer_to_string(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("io__buffer_to_string/2").

:- pred io__read_into_buffer(stream::in, buffer::di, int::in, int::in,
		    buffer::uo, int::out, io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__read_into_buffer(Stream::in, Buffer0::di, Pos0::in, Size::in,
		    Buffer::uo, Pos::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	MercuryFile *f = (MercuryFile *) Stream;
	char *buffer = (MR_Char *) Buffer0;
	int items_read;

	items_read = MR_READ(*f, buffer + Pos0, Size - Pos0);

	Buffer = (MR_Word) buffer;
	Pos = Pos0 + items_read;
	update_io(IO0, IO);
}").


io__read_into_buffer(_, _, _, _, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__read_into_buffer") }.

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

io__read(Result) -->
	term_io__read_term(ReadResult),
	io__get_line_number(LineNumber),
	{ IsAditiTuple = no },
	{ io__process_read_term(IsAditiTuple, ReadResult, LineNumber,
		Result) }.

io__read_from_string_with_int_instead_of_char(FileName, String, Len, Result,
		Posn0, Posn) :-
	IsAditiTuple = yes,
	io__read_from_string(IsAditiTuple, FileName, String, Len, Result,
		Posn0, Posn).

io__read_from_string(FileName, String, Len, Result, Posn0, Posn) :-
	IsAditiTuple = no,
	io__read_from_string(IsAditiTuple, FileName, String, Len,
		Result, Posn0, Posn). 

:- pred io__read_from_string(bool, string, string, int, io__read_result(T),
				posn, posn).
:- mode io__read_from_string(in, in, in, in, out, in, out) is det.

io__read_from_string(IsAditiTuple, FileName, String, Len,
		Result, Posn0, Posn) :-
	parser__read_term_from_string(FileName, String, Len,
		Posn0, Posn, ReadResult),
	Posn = posn(LineNumber, _, _),
	io__process_read_term(IsAditiTuple, ReadResult, LineNumber, Result).

:- pred io__process_read_term(bool, read_term, int, io__read_result(T)).
:- mode io__process_read_term(in, in, in, out) is det.

io__process_read_term(IsAditiTuple, ReadResult, LineNumber, Result) :-
	(	
		ReadResult = term(_VarSet, Term),
		( 
			(
				IsAditiTuple = yes,
				term_to_type_with_int_instead_of_char(Term,
					Type)
			;
				IsAditiTuple = no,
				term_to_type(Term, Type)
			)
		->
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

io__write_many(_Stream, [], IO, IO ).
io__write_many(Stream, [c(C) | Rest] ) -->
	io__write_char(Stream, C),
	io__write_many(Stream, Rest).
io__write_many(Stream, [i(I) | Rest] ) -->
	io__write_int(Stream, I),
	io__write_many(Stream, Rest).
io__write_many(Stream, [s(S) | Rest]) -->
	io__write_string(Stream, S),
	io__write_many(Stream, Rest).
io__write_many(Stream, [f(F) | Rest]) -->
	io__write_float(Stream, F),
	io__write_many(Stream, Rest).

%-----------------------------------------------------------------------------%

% various different versions of io__print

:- pragma export(io__print(in, in(do_not_allow), in, di, uo),
	"ML_io_print_dna_to_stream").
:- pragma export(io__print(in, in(canonicalize), in, di, uo),
	"ML_io_print_can_to_stream").
:- pragma export(io__print(in, in(include_details_cc), in, di, uo),
	"ML_io_print_cc_to_stream").

io__print(Stream, NonCanon, Term) -->
	io__set_output_stream(Stream, OrigStream),
	io__do_print(NonCanon, Term),
	io__set_output_stream(OrigStream, _Stream).

:- pragma export(io__print(in, in, di, uo), "ML_io_print_to_stream").

io__print(Stream, Term) -->
	io__set_output_stream(Stream, OrigStream),
	io__do_print(canonicalize, Term),
	io__set_output_stream(OrigStream, _Stream).

:- pragma export(io__print(in, di, uo), "ML_io_print_to_cur_stream").

io__print(Term) -->
	io__do_print(canonicalize, Term).

io__print_cc(Term) -->
	io__do_print(include_details_cc, Term).

:- pred io__do_print(deconstruct__noncanon_handling, T, io__state, io__state).
:- mode io__do_print(in(do_not_allow), in, di, uo) is det.
:- mode io__do_print(in(canonicalize), in, di, uo) is det.
:- mode io__do_print(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__do_print(in, in, di, uo) is cc_multi.

io__do_print(NonCanon, Term) -->
	% `string', `char' and `univ' are special cases for io__print
	{ type_to_univ(Term, Univ) },
	( { univ_to_type(Univ, String) } ->
		io__write_string(String)
	; { univ_to_type(Univ, Char) } ->
		io__write_char(Char)
	; { univ_to_type(Univ, OrigUniv) } ->
		io__write_univ(OrigUniv)
	;
		io__print_quoted(NonCanon, Term)
	).

:- pred io__print_quoted(deconstruct__noncanon_handling, T,
	io__state, io__state).
:- mode io__print_quoted(in(do_not_allow), in, di, uo) is det.
:- mode io__print_quoted(in(canonicalize), in, di, uo) is det.
:- mode io__print_quoted(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__print_quoted(in, in, di, uo) is cc_multi.

io__print_quoted(NonCanon, Term) -->
	io__do_write(NonCanon, Term).
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

%-----------------------------------------------------------------------------%

% various different versions of io__write

io__write(Stream, NonCanon, X) -->
	io__set_output_stream(Stream, OrigStream),
	io__do_write(NonCanon, X),
	io__set_output_stream(OrigStream, _Stream).

io__write(Stream, X) -->
	io__set_output_stream(Stream, OrigStream),
	io__do_write(canonicalize, X),
	io__set_output_stream(OrigStream, _Stream).

io__write(X) -->
	io__do_write(canonicalize, X).

io__write_cc(X) -->
	io__do_write(include_details_cc, X).

:- pred io__do_write(deconstruct__noncanon_handling, T, io__state, io__state).
:- mode io__do_write(in(do_not_allow), in, di, uo) is det.
:- mode io__do_write(in(canonicalize), in, di, uo) is det.
:- mode io__do_write(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__do_write(in, in, di, uo) is cc_multi.

io__do_write(NonCanon, Term) -->
	{ type_to_univ(Term, Univ) },
	io__do_write_univ(NonCanon, Univ).

%-----------------------------------------------------------------------------%

% various different versions of io__write_univ

io__write_univ(Univ) -->
	io__do_write_univ(canonicalize, Univ).

io__write_univ(Stream, Univ) -->
	io__set_output_stream(Stream, OrigStream),
	io__do_write_univ(canonicalize, Univ),
	io__set_output_stream(OrigStream, _Stream).

io__write_univ(Stream, NonCanon, Univ) -->
	io__set_output_stream(Stream, OrigStream),
	io__do_write_univ(NonCanon, Univ),
	io__set_output_stream(OrigStream, _Stream).

:- pred io__do_write_univ(deconstruct__noncanon_handling, univ,
	io__state, io__state).
:- mode io__do_write_univ(in(do_not_allow), in, di, uo) is det.
:- mode io__do_write_univ(in(canonicalize), in, di, uo) is det.
:- mode io__do_write_univ(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__do_write_univ(in, in, di, uo) is cc_multi.

io__do_write_univ(NonCanon, Univ) -->
	io__get_op_table(OpTable),
	io__do_write_univ(NonCanon, Univ, ops__max_priority(OpTable) + 1).

:- pred io__do_write_univ(deconstruct__noncanon_handling, univ, ops__priority,
	io__state, io__state).
:- mode io__do_write_univ(in(do_not_allow), in, in, di, uo) is det.
:- mode io__do_write_univ(in(canonicalize), in, in, di, uo) is det.
:- mode io__do_write_univ(in(include_details_cc), in, in, di, uo) is cc_multi.
:- mode io__do_write_univ(in, in, in, di, uo) is cc_multi.

io__do_write_univ(NonCanon, Univ, Priority) -->
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
	; { univ_to_type(Univ, TypeDesc) } ->
		io__write_type_desc(TypeDesc)
	; { univ_to_type(Univ, TypeCtorDesc) } ->
		io__write_type_ctor_desc(TypeCtorDesc)
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
		io__write_ordinary_term(NonCanon, Univ, Priority)
	).

:- pred same_array_elem_type(array(T), T).
:- mode same_array_elem_type(unused, unused) is det.
same_array_elem_type(_, _).

:- pred same_private_builtin_type(private_builtin__type_info(T), T).
:- mode same_private_builtin_type(unused, unused) is det.
same_private_builtin_type(_, _).

:- pred io__write_ordinary_term(deconstruct__noncanon_handling, univ,
	ops__priority, io__state, io__state).
:- mode io__write_ordinary_term(in(do_not_allow), in, in, di, uo) is det.
:- mode io__write_ordinary_term(in(canonicalize), in, in, di, uo) is det.
:- mode io__write_ordinary_term(in(include_details_cc), in, in, di, uo)
	is cc_multi.
:- mode io__write_ordinary_term(in, in, in, di, uo) is cc_multi.

io__write_ordinary_term(NonCanon, Univ, Priority) -->
	{ univ_value(Univ) = Term },
	{ deconstruct__deconstruct(Term, NonCanon, Functor, _Arity, Args) },
	io__get_op_table(OpTable),
	(
		{ Functor = "[|]" },
		{ Args = [ListHead, ListTail] }
	->
		io__write_char('['),
		io__write_arg(NonCanon, ListHead),
		io__write_list_tail(NonCanon, ListTail),
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
		io__do_write_univ(NonCanon, BracedTerm),
		io__write_string(" }")
	;
		{ Functor = "{}" },
		{ Args = [BracedHead | BracedTail] }
	->
		io__write_char('{'),
		io__write_arg(NonCanon, BracedHead),
		io__write_term_args(NonCanon, BracedTail),
		io__write_char('}')
	;
		{ Args = [PrefixArg] },
		{ ops__lookup_prefix_op(OpTable, Functor,
			OpPriority, OpAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		term_io__quote_atom(Functor),
		io__write_char(' '),
		{ adjust_priority(OpPriority, OpAssoc, NewPriority) },
		io__do_write_univ(NonCanon, PrefixArg, NewPriority),
		maybe_write_char(')', Priority, OpPriority)
	;
		{ Args = [PostfixArg] },
		{ ops__lookup_postfix_op(OpTable, Functor,
			OpPriority, OpAssoc) }
	->
		maybe_write_char('(', Priority, OpPriority),
		{ adjust_priority(OpPriority, OpAssoc, NewPriority) },
		io__do_write_univ(NonCanon, PostfixArg, NewPriority),
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
		io__do_write_univ(NonCanon, Arg1, LeftPriority),
		( { Functor = "," } ->
			io__write_string(", ")
		;
			io__write_char(' '),
			term_io__quote_atom(Functor),
			io__write_char(' ')
		),
		{ adjust_priority(OpPriority, RightAssoc, RightPriority) },
		io__do_write_univ(NonCanon, Arg2, RightPriority),
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
		io__do_write_univ(NonCanon, Arg1, FirstPriority),
		io__write_char(' '),
		{ adjust_priority(OpPriority, SecondAssoc, SecondPriority) },
		io__do_write_univ(NonCanon, Arg2, SecondPriority),
		maybe_write_char(')', Priority, OpPriority)
	;
		(
			{ Args = [] },
			{ ops__lookup_op(OpTable, Functor) },
			{ Priority =< ops__max_priority(OpTable) }
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
			io__write_arg(NonCanon, X),
			io__write_term_args(NonCanon, Xs),
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

:- pred io__write_list_tail(deconstruct__noncanon_handling, univ,
	io__state, io__state).
:- mode io__write_list_tail(in(do_not_allow), in, di, uo) is det.
:- mode io__write_list_tail(in(canonicalize), in, di, uo) is det.
:- mode io__write_list_tail(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__write_list_tail(in, in, di, uo) is cc_multi.

io__write_list_tail(NonCanon, Univ) -->
	{ Term = univ_value(Univ) },
	{ deconstruct__deconstruct(Term, NonCanon, Functor, _Arity, Args) },
	( { Functor = "[|]", Args = [ListHead, ListTail] } ->
		io__write_string(", "),
		io__write_arg(NonCanon, ListHead),
		io__write_list_tail(NonCanon, ListTail)
	; { Functor = "[]", Args = [] } ->
		[]
	;
		io__write_string(" | "),
		io__do_write_univ(NonCanon, Univ)
	).

:- pred io__write_term_args(deconstruct__noncanon_handling, list(univ),
	io__state, io__state).
:- mode io__write_term_args(in(do_not_allow), in, di, uo) is det.
:- mode io__write_term_args(in(canonicalize), in, di, uo) is det.
:- mode io__write_term_args(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__write_term_args(in, in, di, uo) is cc_multi.

	% write the remaining arguments
io__write_term_args(_, []) --> [].
io__write_term_args(NonCanon, [X|Xs]) -->
	io__write_string(", "),
	io__write_arg(NonCanon, X),
	io__write_term_args(NonCanon, Xs).

:- pred io__write_arg(deconstruct__noncanon_handling, univ,
	io__state, io__state).
:- mode io__write_arg(in(do_not_allow), in, di, uo) is det.
:- mode io__write_arg(in(canonicalize), in, di, uo) is det.
:- mode io__write_arg(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io__write_arg(in, in, di, uo) is cc_multi.

io__write_arg(NonCanon, X) -->
	arg_priority(ArgPriority),
	io__do_write_univ(NonCanon, X, ArgPriority).

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

:- pred io__write_type_desc(type_desc, io__state, io__state).
:- mode io__write_type_desc(in, di, uo) is det.

io__write_type_desc(TypeDesc) -->
	io__write_string(type_name(TypeDesc)).

:- pred io__write_type_ctor_desc(type_ctor_desc, io__state, io__state).
:- mode io__write_type_ctor_desc(in, di, uo) is det.

io__write_type_ctor_desc(TypeCtorDesc) -->
        { type_ctor_name_and_arity(TypeCtorDesc, ModuleName, Name, Arity0) },
	{ ModuleName = "builtin", Name = "func" ->
		% The type ctor that we call `builtin:func/N' takes N + 1
		% type parameters: N arguments plus one return value.
		% So we need to subtract one from the arity here.
		Arity = Arity0 - 1
	;
		Arity = Arity0
	},
	( { ModuleName = "builtin" } ->
		io__format("%s/%d", [s(Name), i(Arity)])
	;
		io__format("%s:%s/%d", [s(ModuleName), s(Name), i(Arity)])
	).

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
	{ TypeInfo = rtti_implementation__unsafe_cast(PrivateBuiltinTypeInfo) },
	io__write_type_desc(TypeInfo).

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
	{ map__search(StreamNames, get_stream_id(Stream), Name1) ->
		Name = Name1
	;
		Name = "<stream name unavailable>"
	},
	io__set_stream_names(StreamNames).

:- pred io__get_stream_names(io__stream_names, io__state, io__state).
:- mode io__get_stream_names(out, di, uo) is det.

:- pred io__set_stream_names(io__stream_names, io__state, io__state).
:- mode io__set_stream_names(in, di, uo) is det.

:- pragma foreign_proc("C", 
		io__get_stream_names(StreamNames::out, IO0::di, IO::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	StreamNames = ML_io_stream_names;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C", 
		io__set_stream_names(StreamNames::in, IO0::di, IO::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ML_io_stream_names = StreamNames;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", 
		io__get_stream_names(StreamNames::out, IO0::di, IO::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	StreamNames = ML_io_stream_names;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", 
		io__set_stream_names(StreamNames::in, IO0::di, IO::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ML_io_stream_names = StreamNames;
	update_io(IO0, IO);
").

io__get_stream_names(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_stream_names") }.

io__set_stream_names(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_stream_names") }.

:- pred io__delete_stream_name(io__stream, io__state, io__state).
:- mode io__delete_stream_name(in, di, uo) is det.

io__delete_stream_name(Stream) -->
	io__get_stream_names(StreamNames0),
	{ map__delete(StreamNames0, get_stream_id(Stream), StreamNames) },
	io__set_stream_names(StreamNames).

:- pred io__insert_stream_name(io__stream, string, io__state, io__state).
:- mode io__insert_stream_name(in, in, di, uo) is det.

io__insert_stream_name(Stream, Name) -->
	io__get_stream_names(StreamNames0),
	{ map__set(StreamNames0, get_stream_id(Stream), Name, StreamNames) },
	io__set_stream_names(StreamNames).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% global state predicates

	% XXX design flaw with regard to unique modes
	% and io__get_globals/3: the `Globals::uo' mode here is a lie.

:- pragma foreign_proc("C", 
		io__get_globals(Globals::uo, IOState0::di, IOState::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Globals = ML_io_user_globals;
	update_io(IOState0, IOState);
").

:- pragma foreign_proc("C", 
		io__set_globals(Globals::di, IOState0::di, IOState::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* XXX need to globalize the memory */
	ML_io_user_globals = Globals;
	update_io(IOState0, IOState);
").

:- pragma foreign_proc("MC++", 
		io__get_globals(Globals::uo, IOState0::di, IOState::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Globals = ML_io_user_globals;
	update_io(IOState0, IOState);
").

:- pragma foreign_proc("MC++", 
		io__set_globals(Globals::di, IOState0::di, IOState::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ML_io_user_globals = Globals;
	update_io(IOState0, IOState);
").

io__set_globals(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_globals") }.

io__get_globals(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_globals") }.

io__progname_base(DefaultName, PrognameBase) -->
	io__progname(DefaultName, Progname),
	{ dir__basename(Progname, PrognameBase) }.


	% XXX we call a pred version of io__get_stream_id, which is a
	% bit inelegant.  We should either fix the MC++ interface so you
	% can implement functions, or implement everything in this
	% module in C#.

io__get_stream_id(Stream) = Id :- io__get_stream_id(Stream, Id).

:- pred io__get_stream_id(io__stream::in, io__stream_id::out) is det.

:- pragma foreign_proc("C",
	io__get_stream_id(Stream::in, Id::out), 
		[will_not_call_mercury, promise_pure], "

#ifndef MR_NATIVE_GC
	/* 
	** Most of the time, we can just use the pointer to the stream
	** as a unique identifier.
	*/
	Id = (MR_Word) Stream;
#else
	/* 
	** for accurate GC we embed an ID in the MercuryFile
	** and retrieve it here.
	*/
	Id = ((MercuryFile *) Stream)->id;
#endif
").

:- pragma foreign_proc("MC++",
	io__get_stream_id(Stream::in, Id::out), 
		[will_not_call_mercury, promise_pure], "
	MR_MercuryFile mf = ML_DownCast(MR_MercuryFile,
		MR_word_to_c_pointer(Stream));
	Id = mf->id;
").

io__get_stream_id(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("io__get_stream_id").

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

% statistics reporting predicates

io__report_stats -->
	io__report_stats("standard").

io__report_full_memory_stats -->
	io__report_stats("full_memory_stats").

:- pragma promise_pure(io__report_stats/3).

io__report_stats(Selector) -->
	{ Selector = "standard" ->
		impure report_stats
	; Selector = "full_memory_stats" ->
		impure report_full_memory_stats
	; Selector = "tabling" ->
		impure table_builtin__table_report_statistics
	;
		string__format(
			"io__report_stats: selector `%s' not understood",
			[s(Selector)], Message),
		error(Message)
	}.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% miscellaneous predicates

:- interface.

	% XXX Since on the IL backend pragma export is NYI, this
	% predicate must be placed in the interface.
:- pred io__init_state(io__state, io__state).
:- mode io__init_state(di, uo) is det.

:- implementation.

% for use by the Mercury runtime
:- pragma export(io__init_state(di, uo), "ML_io_init_state").

io__init_state -->
	io__gc_init(type_of(StreamNames), type_of(Globals)),
	{ map__init(StreamNames) },
	{ type_to_univ("<globals>", Globals) },
	io__set_stream_names(StreamNames),
	io__set_op_table(ops__init_mercury_op_table),
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

:- pred io__gc_init(type_desc, type_desc, io__state, io__state).
:- mode io__gc_init(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
		io__gc_init(StreamNamesType::in, UserGlobalsType::in,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* for Windows DLLs, we need to call GC_INIT() from each DLL */
#ifdef MR_CONSERVATIVE_GC
	GC_INIT();
#endif
	MR_add_root(&ML_io_stream_names, (MR_TypeInfo) StreamNamesType);
	MR_add_root(&ML_io_user_globals, (MR_TypeInfo) UserGlobalsType);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", 
		io__gc_init(_StreamNamesType::in, _UserGlobalsType::in,
		IO0::di, IO::uo), [will_not_call_mercury, promise_pure], "
	update_io(IO0, IO);
	ascii_encoder =	new System::Text::ASCIIEncoding();
").

io__gc_init(_, _) --> [].

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
	io__call_system_return_signal(Command, Result0),
	{
		Result0 = ok(exited(Code)),
		Result = ok(Code)
	;
		Result0 = ok(signalled(Signal)),
		string__int_to_string(Signal, SignalStr),
		string__append("system command killed by signal number ",
			SignalStr, ErrMsg),
		Result = error(io_error(ErrMsg))
	;
		Result0 = error(Error),
		Result = error(Error)
	}.
	
io__call_system_return_signal(Command, Result) -->
	io__call_system_code(Command, Code, Msg),
	{ Code = 127 ->
		Result = error(io_error(Msg))
	;
		Result = io__handle_system_command_exit_status(Code)
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

io__get_op_table(ops__init_mercury_op_table) --> [].

io__set_op_table(_OpTable) --> [].

%-----------------------------------------------------------------------------%

% For use by the debugger:

:- pred io__get_io_input_stream_type(type_desc, io__state, io__state).
:- mode io__get_io_input_stream_type(out, di, uo) is det.

:- pragma export(io__get_io_input_stream_type(out, di, uo),
	"ML_io_input_stream_type").

io__get_io_input_stream_type(Type) -->
	io__stdin_stream(Stream),
	{ Type = type_of(Stream) }.

:- pred io__get_io_output_stream_type(type_desc, io__state, io__state).
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

:- pragma foreign_decl("C", "

#include ""mercury_init.h""
#include ""mercury_wrapper.h""
#include ""mercury_type_info.h""
#include ""mercury_library_types.h""
#include ""mercury_file.h""
#include ""mercury_heap.h""
#include ""mercury_misc.h""

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>

#ifdef MR_HAVE_SYS_WAIT_H
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
MercuryFile*	mercury_open(const char *filename, const char *openmode);
void		mercury_io_error(MercuryFile* mf, const char *format, ...);
void		mercury_output_error(MercuryFile* mf);
void		mercury_print_string(MercuryFile* mf, const char *s);
void		mercury_print_binary_string(MercuryFile* mf, const char *s);
int		mercury_getc(MercuryFile* mf);
void		mercury_close(MercuryFile* mf);
int		ML_fprintf(MercuryFile* mf, const char *format, ...);
").


:- pragma foreign_decl("MC++", "

	// XXX currently we only handle text streams.

namespace mercury {
namespace io__cpp_code {
__gc struct MR_MercuryFileStruct {
public:
	// Note that stream reader and writer might be null, if they are
	// currently unused.

	System::IO::Stream 	*stream; // The stream itself
	System::IO::TextReader 	*reader; // A stream reader for reading it
	System::IO::TextWriter 	*writer; // A stream writer for writing it
	int		line_number;
	int		id;

};

typedef __gc struct MR_MercuryFileStruct *MR_MercuryFile;

}
}

	// These macros aren't very safe -- they don't enforce
	// safe casts in anyway.  Make sure you use them for good
	// and not evil.
#define ML_DownCast(Cast, Expr) dynamic_cast<Cast>(Expr)
#define ML_UpCast(Cast, Expr) ((Cast) (Expr))

#define initial_io_state()	0	/* some random number */
#define update_io(r_src, r_dest)	(0)
#define final_io_state(r)


").

:- pragma foreign_code("C", "

MercuryFile mercury_stdin;
MercuryFile mercury_stdout;
MercuryFile mercury_stderr;
MercuryFile mercury_stdin_binary;
MercuryFile mercury_stdout_binary;

MercuryFile *mercury_current_text_input = &mercury_stdin;
MercuryFile *mercury_current_text_output = &mercury_stdout;
MercuryFile *mercury_current_binary_input = &mercury_stdin_binary;
MercuryFile *mercury_current_binary_output = &mercury_stdout_binary;

void
mercury_init_io(void)
{
	MR_mercuryfile_init(stdin, 1, &mercury_stdin);
	MR_mercuryfile_init(stdout, 1, &mercury_stdout);
	MR_mercuryfile_init(stderr, 1, &mercury_stderr);

	MR_mercuryfile_init(NULL, 1, &mercury_stdin_binary);
	MR_mercuryfile_init(NULL, 1, &mercury_stdout_binary);

#if defined(MR_HAVE_FDOPEN) && (defined(MR_HAVE_FILENO) || defined(fileno)) && \
    defined(MR_HAVE_DUP)
	MR_file(mercury_stdin_binary) = fdopen(dup(fileno(stdin)), ""rb"");
	if (MR_file(mercury_stdin_binary) == NULL) {
		MR_fatal_error(""error opening standard input stream in ""
			""binary mode:\\n\\tfdopen() failed: %s"",
			strerror(errno));
	}
	MR_file(mercury_stdout_binary) = fdopen(dup(fileno(stdout)), ""wb"");
	if (MR_file(mercury_stdout_binary) == NULL) {
		MR_fatal_error(""error opening standard output stream in ""
			""binary mode:\\n\\tfdopen() failed: %s"",
			strerror(errno));
	}
#else
	/*
	** XXX Standard ANSI/ISO C provides no way to set stdin/stdout
	** to binary mode.  I guess we just have to punt...
	*/
	MR_file(mercury_stdin_binary) = stdin;
	MR_file(mercury_stdout_binary) = stdout;
#endif
}

").

:- pragma foreign_code("MC++", "

static MR_MercuryFile new_mercury_file(System::IO::Stream *stream,
		int line_number) {
	MR_MercuryFile mf = new MR_MercuryFileStruct();
	mf->stream = stream;
	mf->reader = NULL;
	mf->writer = NULL;
	mf->line_number = line_number;
	mf->id = next_id++;
	return mf;
}

static MR_MercuryFile new_open_mercury_file(System::IO::Stream *stream,
		System::IO::TextReader *reader, System::IO::TextWriter *writer,
		int line_number) {
	MR_MercuryFile mf = new MR_MercuryFileStruct();
	mf->stream = stream;
	mf->reader = reader;
	mf->writer = writer;
	mf->line_number = line_number;
	mf->id = next_id++;
	return mf;
}

	// XXX this will cause problems with GUI programs that have no
	// consoles.

static MR_MercuryFile mercury_stdin =
	new_open_mercury_file(System::Console::OpenStandardInput(),
		System::Console::In, NULL, 1);
static MR_MercuryFile mercury_stdout =
	new_open_mercury_file(System::Console::OpenStandardOutput(),
		NULL, System::Console::Out, 1);
static MR_MercuryFile mercury_stderr =
	new_open_mercury_file(System::Console::OpenStandardError(),
		NULL, System::Console::Error, 1);

static MR_MercuryFile mercury_stdin_binary =
	new_mercury_file(0, 1);
static MR_MercuryFile mercury_stdout_binary =
	new_mercury_file(0, 1);

	// XXX these should not create extra copies, instead we should
	// use the mercury_files above.

static MR_MercuryFile mercury_current_text_input =
	new_open_mercury_file(System::Console::OpenStandardInput(),
		System::Console::In, NULL, 1);
static MR_MercuryFile mercury_current_text_output =
	new_open_mercury_file(System::Console::OpenStandardOutput(),
		NULL, System::Console::Out, 1);
static MR_MercuryFile mercury_current_binary_input =
        new_mercury_file(0, 1);
static MR_MercuryFile mercury_current_binary_output =
        new_mercury_file(0, 1);

static System::IO::IOException *MR_io_exception;

").


:- pragma foreign_code("C", "

MercuryFile*
mercury_open(const char *filename, const char *openmode)
{
	MercuryFile *mf;
	FILE *f;

	f = fopen(filename, openmode);
	if (!f) return NULL;
	mf = MR_GC_NEW(MercuryFile);
	MR_mercuryfile_init(f, 1, mf);
	return mf;
}

").

:- pragma foreign_code("MC++", "

MR_MercuryFile
static mercury_open(MR_String filename, MR_String openmode)
{
        MR_MercuryFile mf = new MR_MercuryFileStruct();
        System::IO::FileMode fa;
        System::IO::Stream *stream = 0;

	try {
			// XXX get this right...
		if (System::String::op_Equality(openmode, ""r"")) {
			fa = System::IO::FileMode::Open;
		} else if (System::String::op_Equality(openmode, ""a"")) {
			fa = System::IO::FileMode::Append;
		} else if (System::String::op_Equality(openmode, ""w"")) {
			fa = System::IO::FileMode::Truncate;
		} else {
			MR_String msg;
			msg = System::String::Concat(
				""foreign code for this function, open mode:"",
				openmode);
			mercury::runtime::Errors::SORRY(msg);

			// fa = XXX;
		}

		if (fa == System::IO::FileMode::Truncate &&
				!System::IO::File::Exists(filename))
		{
			stream = System::IO::File::Create(filename);
		} else {
			stream = System::IO::File::Open(filename, fa);
		}
	} catch (System::IO::IOException* e) {
		MR_io_exception = e;
	}

        if (!stream) {
                return 0;
        } else {
                mf = new_mercury_file(stream, 1);
                return mf;
        }
}

").


:- pred throw_io_error(string::in) is erroneous.
:- pragma export(throw_io_error(in), "ML_throw_io_error").
throw_io_error(Message) :- throw(io_error(Message)).

:- pragma foreign_code("C", "

void
mercury_io_error(MercuryFile* mf, const char *format, ...)
{
	va_list args;
	char message[5000];
	MR_ConstString message_as_mercury_string;

	/* the `mf' parameter is currently not used */

	/* format the error message using vsprintf() */
	va_start(args, format);
	vsprintf(message, format, args);
	va_end(args);

	/* copy the error message to a Mercury string */
	MR_restore_registers(); /* for MR_hp */
	MR_make_aligned_string_copy(message_as_mercury_string, message);
	MR_save_registers(); /* for MR_hp */

	/* call some Mercury code to throw the exception */
	ML_throw_io_error((MR_String) message_as_mercury_string);
}

").

:- pragma foreign_code("C", "

void
mercury_output_error(MercuryFile *mf)
{
	mercury_io_error(mf, ""error writing to output file: %s"",
		strerror(errno));
}

").

:- pragma foreign_code("C", "

void
mercury_print_string(MercuryFile* mf, const char *s)
{
	if (ML_fprintf(mf, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
	while (*s) {
		if (*s++ == '\\n') {
			MR_line_number(*mf)++;
		}
	}
}

").

:- pragma foreign_code("MC++", "

static void
mercury_print_string(MR_MercuryFile mf, MR_String s)
{
	unsigned char ByteArray __gc[] = ascii_encoder->GetBytes(s);
	mf->stream->Write(ByteArray, 0, ByteArray->get_Length());
	mf->stream->Flush();

        for (int i = 0; i < s->Length; i++) {
                if (s->Chars[i] == '\\n') {
                        mf->line_number++;
                }
        }
}

").

:- pragma foreign_code("C", "

void
mercury_print_binary_string(MercuryFile* mf, const char *s)
{
	if (ML_fprintf(mf, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
}

").

:- pragma foreign_code("C", "

int
mercury_getc(MercuryFile* mf)
{
	int c = MR_GETCH(*mf);
	if (c == '\\n') {
		MR_line_number(*mf)++;
	}
	return c;
}

").


:- pragma foreign_code("MC++", "

static void
mercury_print_binary_string(MR_MercuryFile mf, MR_String s)
{
	// XXX we should re-use the same stream writer...
        System::IO::StreamWriter *w = new System::IO::StreamWriter(mf->stream);
        w->Write(s);
        w->Flush();
}

").

:- pragma foreign_code("MC++", "

static int
mercury_getc(MR_MercuryFile mf)
{
        int c = mf->stream->ReadByte();
        if (c == '\\n') {
                mf->line_number++;
        }
        return c;
}

").


%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- pragma foreign_code("C", "

#ifdef MR_NEW_MERCURYFILE_STRUCT

#include <errno.h>

#ifdef EBADF
  #define MR_CLOSED_FILE_ERROR	EBADF
#else
  /* ANSI/ISO C guarantees that EDOM will exist */
  #define MR_CLOSED_FILE_ERROR	EDOM
#endif

static int
ME_closed_stream_close(MR_StreamInfo *info)
{
	errno = MR_CLOSED_FILE_ERROR;
	return EOF;
}

static int
ME_closed_stream_read(MR_StreamInfo *info, void *buffer, size_t size)
{ 
	errno = MR_CLOSED_FILE_ERROR;
	return -1;	/* XXX should this be 0? */
}

static int
ME_closed_stream_write(MR_StreamInfo *info, const void *buffer, size_t size)
{
	errno = MR_CLOSED_FILE_ERROR;
	return -1;	/* XXX should this be 0? */
}

static int
ME_closed_stream_flush(MR_StreamInfo *info)
{
	errno = MR_CLOSED_FILE_ERROR;
	return EOF;
}

static int
ME_closed_stream_ungetch(MR_StreamInfo *info, int ch)
{
	errno = MR_CLOSED_FILE_ERROR;
	return EOF;
}

static int
ME_closed_stream_getch(MR_StreamInfo *info) 
{
	errno = MR_CLOSED_FILE_ERROR;
	return EOF;
}
 
static int
ME_closed_stream_vfprintf(MR_StreamInfo *info, const char *format, va_list ap)
{
	errno = MR_CLOSED_FILE_ERROR;
	return EOF;
}

static int
ME_closed_stream_putch(MR_StreamInfo *info, int ch)
{
	errno = MR_CLOSED_FILE_ERROR;
	return EOF;
}

static const MercuryFile MR_closed_stream = {
	/* stream_type	= */	MR_USER_STREAM,
	/* stream_info	= */	{ NULL },
	/* line_number	= */	0,

	/* close	= */	ME_closed_stream_close,
	/* read		= */	ME_closed_stream_read,
	/* write	= */	ME_closed_stream_write,

	/* flush	= */	ME_closed_stream_flush,
	/* ungetc	= */	ME_closed_stream_ungetch,
	/* getc		= */	ME_closed_stream_getch,
	/* vprintf	= */	ME_closed_stream_vfprintf,
	/* putc		= */	ME_closed_stream_putch
};

#endif /* MR_NEW_MERCURYFILE_STRUCT */

void
mercury_close(MercuryFile* mf)
{
	if (mf == &mercury_stdin ||
	    mf == &mercury_stdout ||
	    mf == &mercury_stderr ||
	    mf == &mercury_stdin_binary ||
	    mf == &mercury_stdout_binary)
	{
		mercury_io_error(mf,
			""attempt to close stdin, stdout or stderr"");	
	} else {
		if (MR_CLOSE(*mf) < 0) {
			mercury_io_error(mf, ""error closing file: %s"",
				strerror(errno));
		}

#ifdef MR_NEW_MERCURYFILE_STRUCT

		/*
		** MR_closed_stream is a dummy stream object containing
		** pointers to functions that always return an error
		** indication.
		** Doing this ensures that future accesses to the file
		** will fail nicely.
		*/
		/*
		** gcc 2.95.2 barfs on `*mf = MR_closed_stream;'
		** so we use MR_memcpy() instead.
		*/
		MR_memcpy(mf, &MR_closed_stream, sizeof(*mf));

/*
** XXX it would be nice to have an autoconf check
** for the GNU libc function fopencookie();
** we could use that to do a similar thing to what
** we do in the MR_NEW_MERCURYFILE_STRUCT case.
*/

/****
#elif defined(HAVE_FOPENCOOKIE)
		MR_file(*mf) = MR_closed_file;
****/

#else

		/*
		** We want future accesses to the file to fail nicely.
		** Ideally they would throw an exception, but that would
		** require a check at every I/O operation, and for simple
		** operations like putchar() or getchar(), that would be
		** too expensive.  Instead we just set the file pointer
		** to NULL; on systems which trap null pointer dereferences,
		** or if library/io.m is compiled with MR_assert assertions
		** enabled (i.e. -DMR_LOWLEVEL_DEBUG), this will ensure that
		** accessing closed files traps immediately rather than
		** causing problems at some later point.
		*/
		MR_mercuryfile_init(NULL, 0, mf);

#endif /* ! MR_NEW_MERCURYFILE_STRUCT */

#ifndef MR_CONSERVATIVE_GC
  		/*
		** For the accurate GC or no GC cases,
		** we need to explicitly deallocate the memory here,
		** to avoid a memory leak.
		** Note that the accurate collector won't reclaim
		** io_streams, since the io__stream type is defined
		** as a foreign_type.
		*/
  		MR_GC_free(mf);
#endif /* !MR_CONSERVATIVE_GC */

	}
}

").

:- pragma foreign_code("MC++", "

static void
mercury_close(MR_MercuryFile mf)
{
        if (mf == mercury_stdin ||
            mf == mercury_stdout ||
            mf == mercury_stderr ||
            mf == mercury_stdin_binary ||
            mf == mercury_stdout_binary)
        {
                // XXX We should throw an exception here.
                ;	
        } else {
                mf->stream->Close();
                mf->stream = NULL;
        }
}

").


:- pragma foreign_code("C", "

int
ML_fprintf(MercuryFile* mf, const char *format, ...)
{
	int rc;
	va_list args;

	va_start(args, format);
	rc = MR_VFPRINTF(*mf, format, args);
	va_end(args);

	return rc;
}

").

/* input predicates */

:- pragma foreign_proc("C", 
	io__read_char_code(File::in, CharCode::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	CharCode = mercury_getc((MercuryFile *) File);
	update_io(IO0, IO);
").

:- pragma foreign_proc("C", 
	io__putback_char(File::in, Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFile* mf = (MercuryFile *) File;
	if (Character == '\\n') {
		MR_line_number(*mf)--;
	}
	/* XXX should work even if ungetc() fails */
	if (MR_UNGETCH(*mf, Character) == EOF) {
		mercury_io_error(mf, ""io__putback_char: ungetc failed"");
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__putback_byte(File::in, Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFile* mf = (MercuryFile *) File;
	/* XXX should work even if ungetc() fails */
	if (MR_UNGETCH(*mf, Character) == EOF) {
		mercury_io_error(mf, ""io__putback_byte: ungetc failed"");
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++", 
	io__read_char_code(File::in, CharCode::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure], "
	MR_MercuryFile mf = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(File));
	CharCode = mercury_getc(mf);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", 
	io__putback_char(File::in, Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure], "{

	MR_MercuryFile mf = ML_DownCast(MR_MercuryFile,
		MR_word_to_c_pointer(File));
	if (Character == '\\n') {
		mf->line_number--;
	}
	mf->stream->Seek(-1, System::IO::SeekOrigin::Current);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__putback_byte(File::in, _Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure], "{

	MR_MercuryFile mf = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(File));
	mf->stream->Seek(-1, System::IO::SeekOrigin::Current);
	update_io(IO0, IO);
}").

io__read_char_code(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__read_char_code") }.

io__putback_char(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__putback_char") }.

io__putback_byte(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__putback_byte") }.

/* output predicates - with output to mercury_current_text_output */

:- pragma foreign_proc("C", 
	io__write_string(Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	mercury_print_string(mercury_current_text_output, Message);
	update_io(IO0, IO);
").

:- pragma foreign_proc("C", 
	io__write_char(Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (MR_PUTCH(*mercury_current_text_output, Character) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	if (Character == '\\n') {
		MR_line_number(*mercury_current_text_output)++;
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_int(Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (ML_fprintf(mercury_current_text_output, ""%ld"", (long) Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_float(Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (ML_fprintf(mercury_current_text_output, ""%#.15g"", Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_byte(Byte::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	/* call putc with a strictly non-negative byte-sized integer */
	if (MR_PUTCH(*mercury_current_binary_output,
			(int) ((unsigned char) Byte)) < 0)
	{
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_bytes(Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	mercury_print_binary_string(mercury_current_binary_output, Message);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C", 
	io__flush_output(IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (MR_FLUSH(*mercury_current_text_output) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__flush_binary_output(IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (MR_FLUSH(*mercury_current_binary_output) < 0) {
		mercury_output_error(mercury_current_binary_output);
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", 
	io__write_string(Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_print_string(mercury_current_text_output, Message);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", 
	io__write_char(Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	System::IO::StreamWriter *w = new System::IO::StreamWriter(
		mercury_current_text_output->stream);
	w->Write(Character);
	w->Flush();
	if (Character == '\\n') {
		mercury_current_text_output->line_number++;
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__write_int(Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_print_string(mercury_current_text_output, Val.ToString());
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__write_float(Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_print_string(mercury_current_text_output, Val.ToString());
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__write_byte(Byte::in, _IO0::di, _IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
		// XXX something like this...
	System::IO::StreamWriter *w = new System::IO::StreamWriter(
		mercury_current_text_output->stream);
	w->Write(Byte.ToString());
	w->Flush();
").

:- pragma foreign_proc("MC++",
	io__write_bytes(Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	mercury_print_binary_string(mercury_current_binary_output, Message);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++", 
	io__flush_output(IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_current_text_output->stream->Flush();
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__flush_binary_output(IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_current_binary_output->stream->Flush();
	update_io(IO0, IO);
").

io__write_string(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_string") }.

io__write_char(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_char") }.

io__write_int(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_int") }.

io__write_float(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_float") }.

io__write_byte(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_byte") }.

io__write_bytes(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_bytes") }.

io__flush_output -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__flush_output") }.

io__flush_binary_output -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__flush_binary_output") }.

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

:- pragma foreign_proc("C",
	io__seek_binary_2(Stream::in, Flag::in, Off::in,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };
	MercuryFile *stream = (MercuryFile *) Stream;
	/* XXX should check for failure */
	/* XXX should also check if the stream is seekable */
	if (MR_IS_FILE_STREAM(*stream)) {
		fseek(MR_file(*stream), Off, seek_flags[Flag]);
	} else {
		mercury_io_error(stream,
				""io__seek_binary_2: unseekable stream"");
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__binary_stream_offset(Stream::in, Offset::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	/* XXX should check for failure */
	/* XXX should check if the stream is tellable */
	if (MR_IS_FILE_STREAM(*stream)) {
		Offset = ftell(MR_file(*stream));
	} else {
		mercury_io_error(stream,
			""io__binary_stream_offset: untellable stream"");
	}
	update_io(IO0, IO);
}").

io__seek_binary_2(_, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__seek_binary_2") }.

io__binary_stream_offset(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__binary_stream_offset") }.

/* output predicates - with output to the specified stream */

:- pragma foreign_proc("C",
	io__write_string(Stream::in, Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe], 
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_char(Stream::in, Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe], 
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (MR_PUTCH(*stream, Character) < 0) {
		mercury_output_error(stream);
	}
	if (Character == '\\n') {
		MR_line_number(*stream)++;
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_int(Stream::in, Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (ML_fprintf(stream, ""%ld"", (long) Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_float(Stream::in, Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (ML_fprintf(stream, ""%#.15g"", Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_byte(Stream::in, Byte::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	/* call putc with a strictly non-negative byte-sized integer */
	if (MR_PUTCH(*stream, (int) ((unsigned char) Byte)) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_bytes(Stream::in, Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_binary_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__flush_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (MR_FLUSH(*stream) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__flush_binary_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (MR_FLUSH(*stream) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__write_string(Stream::in, Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io], 
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	mercury_print_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__write_char(Stream::in, Character::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io], 
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	System::IO::StreamWriter *w = new System::IO::StreamWriter(
		mercury_current_binary_output->stream);
	w->Write(Character);
	w->Flush();
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__write_int(Stream::in, Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	System::IO::StreamWriter *w = new System::IO::StreamWriter(
		mercury_current_binary_output->stream);
	w->Write(Val.ToString());
	w->Flush();
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__write_float(Stream::in, Val::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	System::IO::StreamWriter *w = new System::IO::StreamWriter(
		mercury_current_binary_output->stream);
	w->Write(Val.ToString());
	w->Flush();
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__write_byte(Stream::in, Byte::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
		// something like this...
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	System::IO::StreamWriter *w = new System::IO::StreamWriter(
		mercury_current_binary_output->stream);
	w->Write(Byte.ToString());
	w->Flush();
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__write_bytes(Stream::in, Message::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	mercury_print_binary_string(stream, Message);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__flush_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	stream->stream->Flush();
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__flush_binary_output(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	stream->stream->Flush();
	update_io(IO0, IO);
}").

io__write_string(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_string") }.

io__write_char(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_char") }.

io__write_int(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_int") }.

io__write_float(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_float") }.

io__write_byte(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_byte") }.

io__write_bytes(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__write_bytes") }.

io__flush_output(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__flush_output") }.

io__flush_binary_output(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__flush_binary_output") }.

/* stream predicates */

:- pragma export(io__stdin_stream(out, di, uo), "ML_io_stdin_stream").
:- pragma export(io__stdout_stream(out, di, uo), "ML_io_stdout_stream").
:- pragma export(io__stderr_stream(out, di, uo), "ML_io_stderr_stream").

:- pragma foreign_proc("C",
	io__stdin_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	Stream = (MR_Word) &mercury_stdin;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stdout_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	Stream = (MR_Word) &mercury_stdout;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stderr_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	Stream = (MR_Word) &mercury_stderr;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stdin_binary_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	Stream = (MR_Word) &mercury_stdin_binary;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stdout_binary_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	Stream = (MR_Word) &mercury_stdout_binary;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__input_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) mercury_current_text_input;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__output_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) mercury_current_text_output;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__binary_input_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) mercury_current_binary_input;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__binary_output_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = (MR_Word) mercury_current_binary_output;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__get_line_number(LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = MR_line_number(*mercury_current_text_input);
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__get_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	LineNum = MR_line_number(*stream);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__set_line_number(LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_line_number(*mercury_current_text_input) = LineNum;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	MR_line_number(*stream) = LineNum;
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__get_output_line_number(LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = MR_line_number(*mercury_current_text_output);
	update_io(IO0, IO);
").
	
:- pragma foreign_proc("C",
	io__get_output_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	LineNum = MR_line_number(*stream);
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__set_output_line_number(LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_line_number(*mercury_current_text_output) = LineNum;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_output_line_number(Stream::in, LineNum::in,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFile *stream = (MercuryFile *) Stream;
	MR_line_number(*stream) = LineNum;
	update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__current_input_stream(OutStream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_text_input;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__current_output_stream(OutStream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_text_output;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__current_binary_input_stream(OutStream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_binary_input;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__current_binary_output_stream(OutStream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_binary_output;
	update_io(IO0, IO);
").

% io__set_input_stream(NewStream, OldStream, IO0, IO1)
%	Changes the current input stream to the stream specified.
%	Returns the previous stream.
:- pragma foreign_proc("C",
	io__set_input_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_text_input;
	mercury_current_text_input = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_output_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_text_output;
	mercury_current_text_output = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_binary_input_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_binary_input;
	mercury_current_binary_input = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_binary_output_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = (MR_Word) mercury_current_binary_output;
	mercury_current_binary_output = (MercuryFile *) NewStream;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__stdin_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe,
			tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_stdin);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__stdout_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe,
			tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_stdout);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__stderr_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe,
			tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_stderr);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__stdin_binary_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe,
			tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_stdin_binary);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__stdout_binary_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, thread_safe,
			tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_stdout_binary);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__input_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_current_text_input);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__output_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_current_text_output);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__binary_input_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_current_binary_input);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__binary_output_stream(Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_c_pointer_to_word(Stream, mercury_current_binary_output);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__get_line_number(LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = mercury_current_text_input->line_number;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__get_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	LineNum = stream->line_number;
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__set_line_number(LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	mercury_current_text_input->line_number = LineNum;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__set_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	stream->line_number = LineNum;
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__get_output_line_number(LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], "
	LineNum = mercury_current_text_output->line_number;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__get_output_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], "{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	LineNum = stream->line_number;
	update_io(IO0, IO);
}").

:- pragma foreign_proc("MC++",
	io__set_output_line_number(LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], "
	mercury_current_text_output->line_number = LineNum;
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__set_output_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], "{
	MR_MercuryFile stream = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	stream->line_number = LineNum;
	update_io(IO0, IO);
}").

% io__set_input_stream(NewStream, OldStream, IO0, IO1)
%	Changes the current input stream to the stream specified.
%	Returns the previous stream.
:- pragma foreign_proc("MC++",
	io__set_input_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], "
	MR_c_pointer_to_word(OutStream, mercury_current_text_input);
	mercury_current_text_input = 
		ML_DownCast(MR_MercuryFile, MR_word_to_c_pointer(NewStream));
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__set_output_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], "
	MR_c_pointer_to_word(OutStream, mercury_current_text_output);
	mercury_current_text_output = 
		ML_DownCast(MR_MercuryFile, MR_word_to_c_pointer(NewStream));
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__set_binary_input_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io], "
	MR_c_pointer_to_word(OutStream, mercury_current_binary_input);
	mercury_current_binary_input = 
		ML_DownCast(MR_MercuryFile, MR_word_to_c_pointer(NewStream));
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__set_binary_output_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo), 
		[will_not_call_mercury, promise_pure, tabled_for_io], "
	MR_c_pointer_to_word(OutStream, mercury_current_binary_output);
	mercury_current_binary_output = 
		ML_DownCast(MR_MercuryFile, MR_word_to_c_pointer(NewStream));
	update_io(IO0, IO);
").

io__stdin_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__stdin_stream") }.

io__stdout_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__stdout_stream") }.

io__stderr_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__stderr_stream") }.

io__stdin_binary_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__stdin_binary_stream") }.

io__stdout_binary_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__stdout_binary_stream") }.

io__input_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__input_stream") }.

io__output_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__output_stream") }.

io__binary_input_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__binary_input_stream") }.

io__binary_output_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__binary_output_stream") }.

io__get_line_number(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_line_number") }.

io__get_line_number(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_line_number") }.

io__set_line_number(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_line_number") }.

io__set_line_number(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_line_number") }.

io__get_output_line_number(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_output_line_number") }.
	
io__get_output_line_number(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_output_line_number") }.
	
io__set_output_line_number(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_output_line_number") }.
	
io__set_output_line_number(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_output_line_number") }.
	
io__current_input_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__current_input_stream") }.

io__current_output_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__current_output_stream") }.

io__current_binary_input_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__current_binary_input_stream") }.

io__current_binary_output_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__current_binary_output_stream") }.

io__set_input_stream(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_input_stream") }.

io__set_output_stream(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_output_stream") }.

io__set_binary_input_stream(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_binary_input_stream") }.

io__set_binary_output_stream(_, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_binary_output_stream") }.

/* stream open/close predicates */

% io__do_open(File, Mode, ResultCode, Stream, IO0, IO1).
%	Attempts to open a file in the specified mode.
%	ResultCode is 0 for success, -1 for failure.
:- pragma foreign_proc("C",
	io__do_open(FileName::in, Mode::in, ResultCode::out,
		Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	Stream = (MR_Word) mercury_open(FileName, Mode);
	ResultCode = (Stream ? 0 : -1);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__do_open(FileName::in, Mode::in, ResultCode::out,
		Stream::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	MR_MercuryFile mf = mercury_open(FileName, Mode);
	MR_c_pointer_to_word(Stream, mf);
	ResultCode = (mf ? 0 : -1);
	update_io(IO0, IO);
").

io__do_open(_, _, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__do_open") }.

io__close_input(Stream) -->
	io__delete_stream_name(Stream),
	io__close_stream(Stream).

io__close_output(Stream) -->
	io__delete_stream_name(Stream),
	io__close_stream(Stream).

io__close_binary_input(Stream) -->
	io__delete_stream_name(Stream),
	io__close_stream(Stream).

io__close_binary_output(Stream) -->
	io__delete_stream_name(Stream),
	io__close_stream(Stream).

:- pred io__close_stream(stream::in, io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C", io__close_stream(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe], "
	mercury_close((MercuryFile *) Stream);
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++", io__close_stream(Stream::in, IO0::di, IO::uo),
		[may_call_mercury, promise_pure, tabled_for_io, thread_safe], "
	MR_MercuryFile mf = ML_DownCast(MR_MercuryFile, 
		MR_word_to_c_pointer(Stream));
	mercury_close(mf);
	update_io(IO0, IO);
").

io__close_stream(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__close_stream") }.

/* miscellaneous predicates */

:- pragma foreign_proc("C",
	io__progname(DefaultProgname::in, PrognameOut::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	if (MR_progname) {
		/*
		** The silly casting below is needed to avoid
		** a gcc warning about casting away const.
		** PrognameOut is of type `MR_String' (char *);
		** it should be of type `MR_ConstString' (const char *),
		** but fixing that requires a fair bit of work
		** on the compiler.
		*/
		MR_make_aligned_string(
			MR_LVALUE_CAST(MR_ConstString, PrognameOut),
			MR_progname);
	} else {
		PrognameOut = DefaultProgname;
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__command_line_arguments(Args::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"
	/* convert mercury_argv from a vector to a list */
	{ int i = mercury_argc;
	  Args = MR_list_empty_msg(MR_PROC_LABEL);
	  while (--i >= 0) {
		Args = MR_list_cons_msg((MR_Word) mercury_argv[i], Args,
			MR_PROC_LABEL);
	  }
	}
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__get_exit_status(ExitStatus::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io], 
"
	ExitStatus = mercury_exit_status;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_exit_status(ExitStatus::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	mercury_exit_status = ExitStatus;
	update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__call_system_code(Command::in, Status::out, Msg::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Status = system(Command);
	if ( Status == -1 ) {
		/* 
		** Return values of 127 or -1 from system() indicate that
		** the system call failed. Don't return -1, as -1 indicates
		** that the system call was killed by signal number 1. 
		*/
		Status = 127;
		ML_maybe_make_err_msg(MR_TRUE,
			""error invoking system command: "",
			MR_PROC_LABEL, Msg);
	} else {
		Msg = MR_make_string_const("""");	
	}
	update_io(IO0, IO);
").


io__progname(DefaultProgName::in, ProgName::out, IO::di, IO::uo) :-
	% This is a fall-back for back-ends which don't support the
	% C interface.
	ProgName = DefaultProgName.

io__handle_system_command_exit_status(Code0) = Status :-
	Code = io__handle_system_command_exit_code(Code0),
	( Code = 127 ->
		Status = error(
			io_error("unknown result code from system command"))
	; Code < 0 ->
		Status = ok(signalled(-Code))
	;
		Status = ok(exited(Code))
	).

	% Interpret the child process exit status returned by
	% system() or wait(): return negative for `signalled',
	% non-negative for `exited', or 127 for anything else
	% (e.g. an error invoking the command).
:- func io__handle_system_command_exit_code(int) = int.

% This is a fall-back for back-ends that don't support the C interface.
io__handle_system_command_exit_code(Status0::in) = (Status::out) :-
	( (Status0 /\ 0xff) \= 0 ->
		/* the process was killed by a signal */
		Status = -(Status0 /\ 0xff)
	;
		/* the process terminated normally */
		Status = (Status0 /\ 0xff00) >> 8
	).

:- pragma foreign_proc("C",
	io__handle_system_command_exit_code(Status0::in) = (Status::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	#if defined (WIFEXITED) && defined (WEXITSTATUS) && \
            defined (WIFSIGNALED) && defined (WTERMSIG)
		if (WIFEXITED(Status0)) {
			Status = WEXITSTATUS(Status0);
		} else if (WIFSIGNALED(Status0)) {
			Status = -WTERMSIG(Status0);
		} else {
			Status = 127;
		}
	#else
		if (Status0 & 0xff != 0) {
			/* the process was killed by a signal */
			Status = -(Status0 & 0xff);
		} else {
			/* the process terminated normally */
			Status = (Status0 & 0xff00) >> 8;
		}
	#endif
").

:- pragma foreign_proc("MC++",
	io__command_line_arguments(Args::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe], "
#ifdef MR_HIGHLEVEL_DATA
	mercury::runtime::Errors::SORRY(""io__command_line_arguments"");
#else
	MR_String arg_vector __gc[] = System::Environment::GetCommandLineArgs();
	int i = arg_vector->Length;
	MR_list_nil(Args);
		// We don't get the 0th argument: it is the executable name
	while (--i > 0) {
		MR_list_cons(Args, arg_vector[i], Args);
	}
	update_io(IO0, IO);
#endif
").

:- pragma foreign_proc("MC++",
	io__get_exit_status(ExitStatus::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ExitStatus = System::Environment::get_ExitCode();
	update_io(IO0, IO);
").

:- pragma foreign_proc("MC++",
	io__set_exit_status(ExitStatus::in, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
	System::Environment::set_ExitCode(ExitStatus);
	update_io(IO0, IO);
").

/* XXX Implementation needs to be finished.
:- pragma foreign_proc("MC++",
	io__call_system_code(Command::in, Status::out, _Msg::out,
			IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io],
"
		// XXX This could be better... need to handle embedded spaces.
	MR_Integer index = Command->IndexOf("" "");
	MR_String commandstr = Command->Substring(index);
	MR_String argstr = Command->Remove(0, index);
		// XXX	This seems to be missing...
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
//	Diagnostics::Process::Start(commandstr, argstr);
	Status = NULL;
	update_io(IO0, IO);
").
*/

io__command_line_arguments(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__command_line_arguments") }.

io__get_exit_status(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__get_exit_status") }.

io__set_exit_status(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__set_exit_status") }.

io__call_system_code(_, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__call_system_code") }.

/*---------------------------------------------------------------------------*/

/* io__getenv and io__putenv, from io.m */

:- pragma promise_semipure(io__getenv/2).
:- pragma foreign_proc("C", io__getenv(Var::in, Value::out),
		[will_not_call_mercury, tabled_for_io],
"{
	Value = getenv(Var);
	SUCCESS_INDICATOR = (Value != 0);
}").

:- pragma foreign_proc("C", io__putenv(VarAndValue::in),
		[will_not_call_mercury, tabled_for_io],
"
	SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);
").

:- pragma foreign_proc("MC++", io__getenv(Var::in, Value::out),
		[will_not_call_mercury, tabled_for_io],
"{
	Value = System::Environment::GetEnvironmentVariable(Var);
	SUCCESS_INDICATOR = (Value != 0);
}").

:- pragma foreign_proc("MC++", io__putenv(_VarAndValue::in),
		[will_not_call_mercury, tabled_for_io],
"
	mercury::runtime::Errors::SORRY(
		""No SetEnvironmentVariable method appears to be available."");
	SUCCESS_INDICATOR = 0;
").

io__getenv(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("io__getenv").

io__putenv(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("io__putenv").

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

io__make_temp(Dir, Prefix, Name) -->
	io__do_make_temp(Dir, Prefix, Name, Err, Message),
	{ Err \= 0 ->
		throw_io_error(Message)
	;
		true
	}.

/*---------------------------------------------------------------------------*/

:- pred io__do_make_temp(string, string, string, int, string,
	io__state, io__state).
:- mode io__do_make_temp(in, in, out, out, out, di, uo) is det.

/*
** XXX	The code for io__make_temp assumes POSIX.
**	It uses the functions open(), close(), and getpid()
**	and the macros EEXIST, O_WRONLY, O_CREAT, and O_EXCL.
**	We should be using conditional compilation here to
**	avoid these POSIX dependencies.
*/

%#include <stdio.h>

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_UNISTD_H
	#include <unistd.h>
#endif
	#include <sys/types.h>
	#include <sys/stat.h>
	#include <fcntl.h>

	#define	MAX_TEMPNAME_TRIES	(6 * 4)

	extern long ML_io_tempnam_counter;
").

:- pragma foreign_code("C", "
	long	ML_io_tempnam_counter = 0;
").

:- pragma foreign_proc("C",
	io__do_make_temp(Dir::in, Prefix::in, FileName::out,
		Error::out, ErrorMessage::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
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
	MR_incr_hp_atomic_msg(MR_LVALUE_CAST(MR_Word, FileName),
		(len + sizeof(MR_Word)) / sizeof(MR_Word),
		MR_PROC_LABEL, ""string:string/0"");
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
		ML_maybe_make_err_msg(MR_TRUE, ""error opening temporary file: "",
			MR_PROC_LABEL, ErrorMessage);
		Error = -1;
	}  else {
		err = close(fd);
		ML_maybe_make_err_msg(err, ""error closing temporary file: "",
			MR_PROC_LABEL, ErrorMessage);
		Error = err;
	}
	update_io(IO0, IO);
}").

io__do_make_temp(_, _, _, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__do_make_temp") }.

/*---------------------------------------------------------------------------*/

:- pragma foreign_decl("C", "

#include <string.h>
#include <errno.h>

/*
** ML_maybe_make_err_msg(was_error, msg, procname, error_msg):
**	if `was_error' is true, then append `msg' and `strerror(errno)'
**	to give `error_msg'; otherwise, set `error_msg' to NULL.
**
** WARNING: this must only be called when the `hp' register is valid.
** That means it must only be called from procedures declared
** `[will_not_call_mercury, promise_pure]'.
**
** This is defined as a macro rather than a C function
** to avoid worrying about the `hp' register being
** invalidated by the function call.
** It also needs to be a macro because MR_incr_hp_atomic_msg()
** stringizes the procname argument.
*/
#define ML_maybe_make_err_msg(was_error, msg, procname, error_msg)	\\
	do {								\\
		char *errno_msg;					\\
		size_t total_len;					\\
		MR_Word tmp;						\\
									\\
		if (was_error) {					\\
			errno_msg = strerror(errno);			\\
			total_len = strlen(msg) + strlen(errno_msg);	\\
			MR_incr_hp_atomic_msg(tmp,			\\
				(total_len + sizeof(MR_Word))		\\
					/ sizeof(MR_Word),		\\
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

:- pragma foreign_proc("C",
	io__remove_file_2(FileName::in, RetVal::out, RetStr::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	RetVal = remove(FileName);
	ML_maybe_make_err_msg(RetVal != 0, ""remove failed: "",
		MR_PROC_LABEL, RetStr);
	update_io(IO0, IO);
}").

/* XXX Implementation needs to be finished.
:- pragma foreign_proc("MC++",
	io__remove_file_2(FileName::in, RetVal::out, RetStr::out,
		IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	System::IO::File::Delete(FileName);
	RetVal = 0;
	RetStr = """";
	update_io(IO0, IO);
}").
*/

io__remove_file_2(_, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__remove_file_2") }.

io__rename_file(OldFileName, NewFileName, Result, IO0, IO) :-
	io__rename_file_2(OldFileName, NewFileName, Res, ResString, IO0, IO),
	( Res \= 0 ->
		Result = error(io_error(ResString))
	;
		Result = ok
	).

:- pred io__rename_file_2(string, string, int, string, io__state, io__state).
:- mode io__rename_file_2(in, in, out, out, di, uo) is det.

:- pragma foreign_proc("C",
		io__rename_file_2(OldFileName::in, NewFileName::in,
			RetVal::out, RetStr::out, IO0::di, IO::uo),
		[will_not_call_mercury, promise_pure, tabled_for_io,
			thread_safe],
"{
	RetVal = rename(OldFileName, NewFileName);
	ML_maybe_make_err_msg(RetVal != 0, ""rename failed: "",
		MR_PROC_LABEL, RetStr);
	update_io(IO0, IO);
}").

io__rename_file_2(_, _, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("io__rename_file_2") }.

/*---------------------------------------------------------------------------*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%	Functional forms added.

io__error_message(Error) = Msg :-
	io__error_message(Error, Msg).
