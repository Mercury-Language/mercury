%-----------------------------------------------------------------------------r
% Copyright (C) 1993-2003 The University of Melbourne.
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
:- import_module bool, char, string, std_util, list, map, time, deconstruct.

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

	% a unique identifier for an IO stream
:- type io__stream_id.

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

:- pred io__input_stream_foldl2_io_maybe_stop(
			pred(char, bool, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__input_stream_foldl2_io_maybe_stop(
			(pred(in, out, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__input_stream_foldl2_io_maybe_stop(
			(pred(in, out, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error, or the
%		closure returns `no' as its second argument.


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

:- pred io__input_stream_foldl2_io_maybe_stop(io__input_stream,
			pred(char, bool, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__input_stream_foldl2_io_maybe_stop(in,
			(pred(in, out, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__input_stream_foldl2_io_maybe_stop(in,
			(pred(in, out, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each character read from
%		the input stream in turn, until eof or error, or the
%		closure returns `no' as its second argument.

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

:- pred io__binary_input_stream_foldl(pred(int, T, T),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__binary_input_stream_foldl((pred(in, in, out) is det),
			in, out, di, uo) is det.
:- mode io__binary_input_stream_foldl((pred(in, in, out) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		current binary input stream in turn, until eof or error.

:- pred io__binary_input_stream_foldl_io(pred(int, io__state, io__state),
			io__res, io__state, io__state).
:- mode io__binary_input_stream_foldl_io((pred(in, di, uo) is det),
			out, di, uo) is det.
:- mode io__binary_input_stream_foldl_io((pred(in, di, uo) is cc_multi),
			out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		current binary input stream in turn, until eof or error.

:- pred io__binary_input_stream_foldl2_io(
			pred(int, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__binary_input_stream_foldl2_io((pred(in, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__binary_input_stream_foldl2_io(
			(pred(in, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		current binary input stream in turn, until eof or error.

:- pred io__binary_input_stream_foldl2_io_maybe_stop(
			pred(int, bool, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__binary_input_stream_foldl2_io_maybe_stop(
			(pred(in, out, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__binary_input_stream_foldl2_io_maybe_stop(
			(pred(in, out, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		current binary input stream in turn, until eof or error,
%		or the closure returns `no' as its second argument.

:- pred io__binary_input_stream_foldl(io__binary_input_stream,
			pred(int, T, T), T, io__maybe_partial_res(T),
			io__state, io__state).
:- mode io__binary_input_stream_foldl(in, (pred(in, in, out) is det),
			in, out, di, uo) is det.
:- mode io__binary_input_stream_foldl(in, (pred(in, in, out) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		given binary input stream in turn, until eof or error.

:- pred io__binary_input_stream_foldl_io(io__binary_input_stream,
			pred(int, io__state, io__state), io__res,
			io__state, io__state).
:- mode io__binary_input_stream_foldl_io(in, (pred(in, di, uo) is det),
			out, di, uo) is det.
:- mode io__binary_input_stream_foldl_io(in, (pred(in, di, uo) is cc_multi),
			out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		given binary input stream in turn, until eof or error.

:- pred io__binary_input_stream_foldl2_io(io__binary_input_stream,
			pred(int, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__binary_input_stream_foldl2_io(in,
			(pred(in, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__binary_input_stream_foldl2_io(in,
			(pred(in, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		given binary input stream in turn, until eof or error.

:- pred io__binary_input_stream_foldl2_io_maybe_stop(
			io__binary_input_stream,
			pred(int, bool, T, T, io__state, io__state),
			T, io__maybe_partial_res(T), io__state, io__state).
:- mode io__binary_input_stream_foldl2_io_maybe_stop(in,
			(pred(in, out, in, out, di, uo) is det),
			in, out, di, uo) is det.
:- mode io__binary_input_stream_foldl2_io_maybe_stop(in,
			(pred(in, out, in, out, di, uo) is cc_multi),
			in, out, di, uo) is cc_multi.
%		Applies the given closure to each byte read from the
%		given binary input stream in turn, until eof or error,
%		or the closure returns `no' as its second argument.

:- pred io__putback_byte(int, io__state, io__state).
:- mode io__putback_byte(in, di, uo) is det.
%		Un-reads a byte from the current binary input stream.
%		You can put back as many bytes as you like.
%		You can even put back something that you didn't actually read.
%		The byte is taken from the bottom 8 bits of an integer.
%		Note: `io__putback_byte' uses the C library function ungetc().
%		On some systems only one byte of pushback is guaranteed.
%		`io__putback_byte' will throw an io__error exception
%		if ungetc() fails.

:- pred io__putback_byte(io__binary_input_stream, int, io__state, io__state).
:- mode io__putback_byte(in, in, di, uo) is det.
%		Un-reads a byte from specified binary input stream.
%		You can put back as many bytes as you like.
%		You can even put back something that you didn't actually read.
%		The byte is returned in the bottom 8 bits of an integer.
%		Note: `io__putback_byte' uses the C library function ungetc().
%		On some systems only one byte of pushback is guaranteed.
%		`io__putback_byte' will throw an io__error exception
%		if ungetc() fails.

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

:- pred io__make_temp(string, io__state, io__state).
:- mode io__make_temp(out, di, uo) is det.
	% io__make_temp(Name, IO0, IO) creates an empty file
	% whose name which is different to the name of any existing file.
	% Name is bound to the name of the file.
	% On Microsoft Windows systems, the file will reside in the current
	% directory if the TMP environment variable is not set, or in the
	% directory specified by TMP if it is set.
	% On other systems, the file will reside in /tmp if the TMPDIR
	% environment variable is not set, or in the directory specified
	% by TMPDIR if it is set.
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

:- pred io__have_symlinks is semidet.
	% Can this platform read and create symbolic links.

:- pred io__make_symlink(string, string, io__res, io__state, io__state).
:- mode io__make_symlink(in, in, out, di, uo) is det.
	% io__make_symlink(FileName, LinkFileName, Result, IO0, IO)
	% attempts to make `LinkFileName' be a symbolic link to `FileName'.
	% If `FileName' is a relative path, it is interpreted relative
	% to the directory containing `LinkFileName'.

:- pred io__read_symlink(string, io__res(string), io__state, io__state).
:- mode io__read_symlink(in, out, di, uo) is det.
	% io__read_symlink(FileName, Result, IO0, IO) returns
	% `ok(LinkTarget)' if `FileName' is a symbolic link pointing
	% to `LinkTarget', and `error(Error)' otherwise.
	% If `LinkTarget' is a relative path, it should be interpreted
	% relative the directory containing `FileName', not the current
	% directory.

:- type io__access_type
	--->	read
	;	write
	;	execute
	.

:- pred io__check_file_accessibility(string, list(access_type),
		io__res, io__state, io__state).
:- mode io__check_file_accessibility(in, in, out, di, uo) is det.
	% io__check_file_accessibility(FileName, AccessTypes, Result)
	% Check whether the current process can perform the operations
	% given in `AccessTypes' on `FileName'.
	% XXX When using the .NET CLI, this predicate will sometimes
	% report that a directory is writable when in fact it is not.

:- type io__file_type
	--->	regular_file
	;	directory
	;	symbolic_link
	;	named_pipe
	;	socket
	;	character_device
	;	block_device
	;	message_queue
	;	semaphore
	;	shared_memory
	;	unknown
	. 

:- pred io__file_type(bool, string, io__res(file_type), io__state, io__state).
:- mode io__file_type(in, in, out, di, uo) is det.
	% io__file_type(FollowSymLinks, FileName, TypeResult)
	% finds the type of the given file.

:- pred io__file_modification_time(string, io__res(time_t),
		io__state, io__state).
:- mode io__file_modification_time(in, out, di, uo) is det.
	% io__file_modification_time(FileName, TimeResult)
	% finds the last modification time of the given file.

%-----------------------------------------------------------------------------%

% Memory management predicates.

	% Write memory/time usage statistics to stderr.

:- pred io__report_stats(io__state, io__state).
:- mode io__report_stats(di, uo) is det.

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
%		indicating which signal occurred.

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

:- func io__make_io_error(string) = io__error.
%	io__make_io_error(ErrorMessage) = ErrorCode.
%		Construct an error code including the specified error message.

:- func io__error_message(io__error) = string.
:- pred io__error_message(io__error, string).
:- mode io__error_message(in, out) is det.
%	io__error_message(ErrorCode, ErrorMessage).
%		Look up the error message corresponding to a particular error
%		code.

%-----------------------------------------------------------------------------%
%
% Deprecated predicates.
%
% Do not use these in new programs!
% They may be deleted in the next release.

	% use io__input_stream/3 instead -- it has identical semantics
:- pragma obsolete(io__current_input_stream/3).
:- pred io__current_input_stream(io__input_stream, io__state, io__state).
:- mode io__current_input_stream(out, di, uo) is det.

	% use io__output_stream/3 instead -- it has identical semantics
:- pragma obsolete(io__current_output_stream/3).
:- pred io__current_output_stream(io__output_stream, io__state, io__state).
:- mode io__current_output_stream(out, di, uo) is det.

	% use io__binary_input_stream/3 instead -- it has identical semantics
:- pragma obsolete(io__current_binary_input_stream/3).
:- pred io__current_binary_input_stream(io__binary_input_stream,
			io__state, io__state).
:- mode io__current_binary_input_stream(out, di, uo) is det.

	% use io__binary_output_stream/3 instead -- it has identical semantics
:- pragma obsolete(io__current_binary_output_stream/3).
:- pred io__current_binary_output_stream(io__binary_output_stream,
			io__state, io__state).
:- mode io__current_binary_output_stream(out, di, uo) is det.

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

	% OBSOLETE: call io__report_stats/3 instead, with the first argument
	% specified as "full_memory_stats".
:- pragma obsolete(io__report_full_memory_stats/2).
:- pred io__report_full_memory_stats(io__state, io__state).
:- mode io__report_full_memory_stats(di, uo) is det.
	% Write complete memory usage statistics to stderr,
	% including information about all procedures and types.
	% (You need to compile with memory profiling enabled.)

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%
:- interface.

%
% For use by dir.m:
%

	% A system-dependent error indication.
	% For C, this is the value of errno.
:- type io__system_error.
:- pragma foreign_type(c, io__system_error, "MR_Integer").
:- pragma foreign_type(il, io__system_error,
		"class [mscorlib]System.Exception").

% io__make_err_msg(Error, MessagePrefix, Message):
%	`Message' is an error message obtained by looking up the
%	message for the given errno value and prepending
%	`MessagePrefix'.
:- pred io__make_err_msg(io__system_error, string, string,
		io__state, io__state).
:- mode io__make_err_msg(in, in, out, di, uo) is det.

% Succeeds iff the Win32 API is available.
:- pred have_win32 is semidet.

% Succeeds iff the current process was compiled against the Cygwin library.
:- pred have_cygwin is semidet.

% Succeeds iff the .NET class library is available.
:- pred have_dotnet is semidet.

% io__make_win32_err_msg(Error, MessagePrefix, Message):
%	`Message' is an error message obtained by looking up the
%	error message for the given Win32 error number and prepending
%	`MessagePrefix'.
%	This will abort if called on a system which does not support
%	the Win32 API.
:- pred io__make_win32_err_msg(io__system_error,
		string, string, io__state, io__state).
:- mode io__make_win32_err_msg(in, in, out, di, uo) is det.

% io__make_maybe_win32_err_msg(Error, MessagePrefix, Message):
%	`Message' is an error message obtained by looking up the
%	last Win32 error message and prepending `MessagePrefix'.
%	On non-Win32 systems, the message corresponding to the
%	current value of errno will be used.
:- pred io__make_maybe_win32_err_msg(io__system_error,
		string, string, io__state, io__state).
:- mode io__make_maybe_win32_err_msg(in, in, out, di, uo) is det.

% io__file_id(FileName, FileId).
%
%	Return a unique identifier for the given file (after following
%	symlinks in FileName).
%	XXX On Cygwin sometimes two files will have the same file_id.
%	This is because MS-Windows does not use inodes, so Cygwin
%	hashes the absolute file name.
%	On Windows without Cygwin this will always return error(_).
%	That doesn't matter, because this function is only used for
%	checking for symlink loops in dir.foldl2, but plain Windows
%	doesn't support symlinks.
:- type file_id.
:- pred io__file_id(string, io__res(file_id), io__state, io__state).
:- mode io__file_id(in, out, di, uo) is det.

% Succeeds if io__file_id is implemented on this platform.
:- pred io__have_file_ids is semidet.

%
% For use by term_io.m:
%

:- import_module ops.

:- pred io__get_op_table(ops__table, io__state, io__state).
:- mode io__get_op_table(out, di, uo) is det.

:- pred io__set_op_table(ops__table, io__state, io__state).
:- mode io__set_op_table(di, di, uo) is det.

%
% For use by browser/browse.m:
%

% Types and predicates for managing the stream info database.

:- type io__stream_db ==	map(io__stream_id, stream_info).

:- type stream_info
	--->	stream(
			stream_id		:: int,
			stream_mode		:: stream_mode,
			stream_content		:: stream_content,
			stream_source		:: stream_source
		).

:- type maybe_stream_info
	--->	stream(
			maybe_stream_id		:: int,
			maybe_stream_mode	:: stream_mode,
			maybe_stream_content	:: stream_content,
			maybe_stream_source	:: stream_source
		)
	;	unknown_stream.

:- type stream_mode	--->	input
			;	output
			;	append.

:- type stream_content	--->	text
			;	binary
			;	preopen.

:- type stream_source	--->	file(string)	% the file name
			;	stdin
			;	stdout
			;	stderr.

:- pred io__get_stream_db(io__stream_db::out, io__state::di, io__state::uo)
	is det.
% Retrieves the database mapping streams to the information we have
% about those streams.

:- func io__input_stream_info(io__stream_db, io__input_stream)
	= io__maybe_stream_info.
%	Returns the information associated with the specified input
%	stream in the given stream database.

:- func io__output_stream_info(io__stream_db, io__output_stream)
	= io__maybe_stream_info.
%	Returns the information associated with the specified output
%	stream in the given stream database.

:- func io__binary_input_stream_info(io__stream_db, io__binary_input_stream)
	= io__maybe_stream_info.
%	Returns the information associated with the specified binary input
%	stream in the given stream database.

:- func io__binary_output_stream_info(io__stream_db, io__binary_output_stream)
	= io__maybe_stream_info.
%	Returns the information associated with the specified binary output
%	stream in the given stream database.

% Predicates for writing out univs

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

%
% For use by extras/aditi/aditi.m
%

	% This is the same as io__read_from_string, except that an integer
	% is allowed where a character is expected. This is needed because
	% Aditi does not have a builtin character type. This also allows an
	% integer where a float is expected.
:- pred io__read_from_string_with_int_instead_of_char(string, string, int,
			io__read_result(T), posn, posn).
:- mode io__read_from_string_with_int_instead_of_char(in, in, in,
			out, in, out) is det.

%
% For use by compiler/process_util.m:
%

	% Interpret the child process exit status returned by
	% system() or wait().
:- func io__handle_system_command_exit_status(int) =
		io__res(io__system_result).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, dir, term, term_io, varset, require, benchmarking, array.
:- import_module bool, enum, int, parser, exception.
:- use_module table_builtin.
:- use_module rtti_implementation.

:- pragma foreign_import_module(c, string).

:- type io__state ---> io__state(c_pointer).
	% Values of type `io__state' are never really used:
	% instead we store data in global variables.
	% The reason this is not defined simply as `io__state == c_pointer'
	% is so that `type_name' produces more informative results
	% for cases such as `type_name(main)'.

:- pragma foreign_decl("C", "
	extern MR_Word		ML_io_stream_db;
	extern MR_Word		ML_io_user_globals;
	extern int		ML_next_stream_id;
	#if 0
	  extern MR_Word	ML_io_ops_table;
	#endif
").

:- pragma foreign_code("C", "
	MR_Word			ML_io_stream_db;
	MR_Word			ML_io_user_globals;
	/* a counter used to generate unique stream ids */
	int			ML_next_stream_id;
	#if 0
	  MR_Word		ML_io_ops_table;
	#endif
").

:- pragma foreign_code("C#", "
	// The ML_ prefixes here are not really needed,
	// since the C# code all gets generated inside a class,
	// but we keep them for consistency with the C code.

#if MR_HIGHLEVEL_DATA
	static mercury.tree234.tree234_2	ML_io_stream_db;
	static object[]				ML_io_user_globals;
#else
	static object[]				ML_io_stream_db;
	static object[]				ML_io_user_globals;
#endif

	// a counter used to generate unique stream ids
	static int				ML_next_stream_id;

	// This specifies the default encoding used for text files.
	// It must be either ML_OS_text_encoding or ML_Unix_text_encoding.
	//
	// XXX The initial setting for this should be controlled
	//     by an environment variable.  (This might require moving
	//     the code which initializes mercury_stdin, etc.)
	//
	static ML_file_encoding_kind ML_default_text_encoding =
		ML_file_encoding_kind.ML_OS_text_encoding;
").


:- type io__stream_putback ==	map(io__stream_id, list(char)).

:- type io__input_stream ==	io__stream.
:- type io__output_stream ==	io__stream.

:- type io__binary_stream ==	io__stream.

:- type io__stream --->		io__stream(c_pointer).
:- pragma foreign_type("C", io__stream, "MercuryFilePtr").
:- pragma foreign_type("il", io__stream, "class [mercury]mercury.io__csharp_code.MR_MercuryFileStruct").

	% a unique identifier for an IO stream
:- type io__stream_id == int.

:- func io__get_stream_id(io__stream) = io__stream_id.

	% This inter-language stuff is tricky.
	% We communicate via ints rather than via io__result_codes because
	% we don't want the C code to depend on how Mercury stores its
	% discriminated union data types.

:- pred io__read_char_code(io__input_stream, int, io__state, io__state).
:- mode io__read_char_code(in, out, di, uo) is det.
%		Reads a character from specified stream,
%		and returns the numerical value for that character
%		(as from char__to_int).
%		This may involve converting external character encodings
%		into Mercury's internal character repesentation
%		and (for text streams) converting OS line indicators,
%		e.g. CR-LF for Windows, to '\n' characters.
%		Returns -1 if at EOF, -2 if an error occurs.

:- pred io__read_byte_val(io__input_stream, int, io__state, io__state).
:- mode io__read_byte_val(in, out, di, uo) is det.
%		Reads a byte from specified stream.
%		Returns -1 if at EOF, -2 if an error occurs.

:- pred io__call_system_code(string, int, string, io__state, io__state).
:- mode io__call_system_code(in, out, out, di, uo) is det.
%	io__call_system_code(Command, Status, Message, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Returns Status = 127 and Message on failure.
%		Otherwise returns the raw exit status from the system()
%		call.

:- semipure pred io__getenv(string, string).
:- mode io__getenv(in, out) is semidet.
%	io__getenv(Var, Value).
%		Gets the value Value associated with the environment
%		variable Var.  Fails if the variable was not set.

:- impure pred io__setenv(string, string).
:- mode io__setenv(in, in) is semidet.
%	io__setenv(NameString,ValueString).
%		Sets the named environment variable to the specified
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
	io__read_byte_val(Stream, Code),
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
	io__read_line_as_string_2(Stream, yes, Res, String, IO0, IO1),
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

:- pred io__read_line_as_string_2(io__input_stream, bool, int, string,
		io__state, io__state).
:- mode io__read_line_as_string_2(in, in, out, out, di, uo) is det.

:- pragma foreign_proc("C",
	io__read_line_as_string_2(File::in, _Bool::in, Res :: out,
		RetString::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
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
		char_code = mercury_getc(File);
		if (char_code == EOF) {
			if (i == 0) {
				Res = -1;
			}
			break;
		}
		if (char_code != (MR_UnsignedChar) char_code) {
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
		MR_offset_incr_hp_atomic_msg(MR_LVALUE_CAST(MR_Word, RetString),
			0, ML_IO_BYTES_TO_WORDS((i + 1) * sizeof(MR_Char)),
			MR_PROC_LABEL, ""string:string/0"");
		memcpy(RetString, read_buffer, i * sizeof(MR_Char));
		RetString[i] = '\\0';
	} else {
		RetString = NULL;
	}
	if (read_buffer != initial_read_buffer) {
		MR_free(read_buffer);
	}
	MR_update_io(IO0, IO);
").

	% XXX This is terribly inefficient, a better approach would be to
	% use a buffer like what is done for io__read_file_as_string.
io__read_line_as_string_2(Stream, FirstCall, Res, String) -->
	io__read_char(Stream, Result),
	( { Result = ok(Char) },
		( { Char = '\n' } ->
			{ Res = 0 },
			{ String = "\n" }
		;
			io__read_line_as_string_2(Stream, no, Res, String0),
			{ string__first_char(String, Char, String0) } 
		)
	; { Result = eof },
		{ FirstCall = yes ->
			String = "",
			Res = -1
		;
			String = "",
			Res = 0
		}
	; { Result = error(_) },
		{ String = "" },
		{ Res = -2 }
	).

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
		BufferSize0 = FileSize + 1
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
:- mode io__read_file_as_string_2(in, buffer_di, in, in,
		buffer_uo, out, out, di, uo) is det.

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
		{ Size1 = Size0 * 2 },
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

io__input_stream_foldl2_io_maybe_stop(Pred, T0, Res) -->
	io__input_stream(Stream),
	io__input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res).

io__input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res) -->
	io__read_char(Stream, CharResult),
	(
		{ CharResult = ok(Char) },
		Pred(Char, Continue, T0, T1),
		(
			{ Continue = no },
			{ Res = ok(T1) }
		;
			{ Continue = yes },
			io__input_stream_foldl2_io_maybe_stop(Stream,
				Pred, T1, Res)
		)
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

:- pragma foreign_proc("C",
	io__clear_err(Stream::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	if (MR_IS_FILE_STREAM(*Stream)) {
		clearerr(MR_file(*Stream));
	} else {
		/* Not a file stream so do nothing */
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__clear_err(_Stream::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe],
"{
	// XXX no error flag to reset as in .NET an error is thrown
	// directly as an exception (we should create an error indicator
	// in MF_Mercury_file for compatibility)
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

:- pragma foreign_proc("C",
	ferror(Stream::in, RetVal::out, RetStr::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	if (MR_IS_FILE_STREAM(*Stream)) {
		RetVal = ferror(MR_file(*Stream));
	} else {
		RetVal = -1;
	}

	ML_maybe_make_err_msg(RetVal != 0, errno, ""read failed: "",
		MR_PROC_LABEL, RetStr);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	ferror(_Stream::in, RetVal::out, _RetStr::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe],
"{
	// XXX see clearerr
	RetVal = 0;
}").

:- pred io__make_err_msg(string, string, io__state, io__state).
:- mode io__make_err_msg(in, out, di, uo) is det.

io__make_err_msg(Msg0, Msg) -->
	io__get_system_error(Error),
	io__make_err_msg(Error, Msg0, Msg).

:- pred io__get_system_error(io__system_error, io__state, io__state).
:- mode io__get_system_error(out, di, uo) is det.

:- pragma foreign_proc("C",
	io__get_system_error(Error::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	Error = errno;
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__get_system_error(Error::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	Error = MR_io_exception;
}").

:- pragma export(make_err_msg(in, in, out, di, uo), "ML_make_err_msg").
:- pragma foreign_proc("C",
	make_err_msg(Error::in, Msg0::in, Msg::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	ML_maybe_make_err_msg(MR_TRUE, Error, Msg0, MR_PROC_LABEL, Msg);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#", 
	make_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	Msg = System.String.Concat(Msg0, Error.Message);
}").

have_win32 :- semidet_fail.

:- pragma foreign_proc("C",
	have_win32,
	[will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_WIN32
  SUCCESS_INDICATOR = MR_TRUE;
#else
  SUCCESS_INDICATOR = MR_FALSE;
#endif
").

have_cygwin :- semidet_fail.

:- pragma foreign_proc("C",
	have_cygwin,
	[will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef __CYGWIN__
  SUCCESS_INDICATOR = MR_TRUE;
#else
  SUCCESS_INDICATOR = MR_FALSE;
#endif
").

have_dotnet :- semidet_fail.

:- pragma foreign_proc("C#",
	have_dotnet,
	[will_not_call_mercury, promise_pure, thread_safe],
	"SUCCESS_INDICATOR = true;").

:- pragma export(make_win32_err_msg(in, in, out, di, uo),
		"ML_make_win32_err_msg").

make_win32_err_msg(_, _, _, _, _) :-
	error("io__make_win32_err_msg called for non Win32 back-end").

:- pragma foreign_proc("C",
	make_win32_err_msg(Error::in, Msg0::in, Msg::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	ML_maybe_make_win32_err_msg(MR_TRUE, Error, Msg0, MR_PROC_LABEL, Msg);
	MR_update_io(IO0, IO);
}").

make_maybe_win32_err_msg(Error, Msg0, Msg, !IO) :-
	( have_win32 ->
		make_win32_err_msg(Error, Msg0, Msg, !IO)
	;
		make_err_msg(Error, Msg0, Msg, !IO)
	).		

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
#include ""mercury_types.h""		/* for MR_Integer */
#include ""mercury_library_types.h""	/* for MercuryFilePtr */
").

:- pragma foreign_proc("C",
	io__stream_file_size(Stream::in, Size::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#if defined(MR_HAVE_FSTAT) && \
    (defined(MR_HAVE_FILENO) || defined(fileno)) && \
    defined(S_ISREG)
	struct stat s;
	if (MR_IS_FILE_STREAM(*Stream)) {
		if (fstat(fileno(MR_file(*Stream)), &s) == 0 &&
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
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe],
"{
	if (Stream.stream.CanSeek) {
		Size = (int) Stream.stream.Length;
	} else {
		Size = -1;
	}
}").

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

:- pragma foreign_proc("C",
	io__file_modification_time_2(FileName::in, Status::out, Msg::out,
		Time::out, IO0::di, IO::uo),
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
		ML_maybe_make_err_msg(MR_TRUE, errno, ""stat() failed: "",
			MR_PROC_LABEL, Msg);
		Status = 0;
	}
#else
	Status = 0;
	Msg = MR_make_string_const(
	""io__file_modification_time not available on this platform"");
#endif
	MR_update_io(IO0, IO);

}").
:- pragma foreign_proc("C#",
	io__file_modification_time_2(FileName::in, Status::out, Msg::out,
		Time::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	try {
		System.DateTime t = System.IO.File.GetLastWriteTime(FileName);
		Time = mercury.time.mercury_code.construct_time_t_2(t);
		Msg = """";
		Status = 1;

	} catch (System.Exception e) {
		Msg = ""GetLastWriteTime() failed: "" + e.Message;
		Status = 0;
	}
}").



%-----------------------------------------------------------------------------%

io__file_type(FollowSymLinks, FileName, MaybeType) -->
	( { file_type_implemented } -> 
		{ FollowSymLinksInt = ( FollowSymLinks = yes -> 1 ; 0 ) },
		io__file_type_2(FollowSymLinksInt, FileName, MaybeType)
	;
		{ MaybeType = error(io__make_io_error(
		"Sorry, io.file_type not implemented on this platform")) }
	).

:- pred file_type_implemented is semidet.

file_type_implemented :- semidet_fail.
:- pragma foreign_proc("C", file_type_implemented,
	[will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HAVE_STAT
	SUCCESS_INDICATOR = MR_TRUE;
#else
	SUCCESS_INDICATOR = MR_FALSE;
#endif
").
:- pragma foreign_proc("C#", file_type_implemented,
	[will_not_call_mercury, promise_pure, thread_safe],
	"SUCCESS_INDICATOR = true;"
).

:- pred io__file_type_2(int, string, io__res(io__file_type),
		io__state, io__state).
:- mode io__file_type_2(in, in, out, di, uo) is det.

:- pragma foreign_proc("C",
	io__file_type_2(FollowSymLinks::in, FileName::in,
		Result::out, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_STAT
	struct stat s;
	int stat_result;

	if (FollowSymLinks == 1) {
		stat_result = stat(FileName, &s);
	} else {
		#ifdef MR_HAVE_LSTAT
			stat_result = lstat(FileName, &s);
		#else
			stat_result = stat(FileName, &s);
		#endif
	}

	if (stat_result == 0) {
		MR_Word type;

		#if defined(S_ISREG)
			if (S_ISREG(s.st_mode)) {
				type = ML_file_type_regular();
			} else
		#elif defined(S_IFMT) && defined(S_IFREG)
			if ((s.st_mode & S_IFMT) == S_IFREG) {
				type = ML_file_type_regular();
			} else
		#endif

		#if defined(S_ISDIR)
			if (S_ISDIR(s.st_mode)) {
				type = ML_file_type_directory();
			} else
		#elif defined(S_IFMT) && defined(S_IFDIR)
			if ((s.st_mode & S_IFMT) == S_IFDIR) {
				type = ML_file_type_directory();
			} else
		#endif

		#if defined(S_ISBLK)
			if (S_ISBLK(s.st_mode)) {
				type = ML_file_type_block_device();
			} else
		#elif defined(S_IFMT) && defined(S_IFBLK)
			if ((s.st_mode & S_IFMT) == S_IFBLK) {
				type = ML_file_type_block_device();
			} else
		#endif

		#if defined(S_ISCHR)
			if (S_ISCHR(s.st_mode)) {
				type = ML_file_type_character_device();
			} else
		#elif defined(S_IFMT) && defined(S_IFCHR)
			if ((s.st_mode & S_IFMT) == S_IFCHR) {
				type = ML_file_type_character_device();
			} else
		#endif

		#if defined(S_ISFIFO)
			if (S_ISFIFO(s.st_mode)) {
				type = ML_file_type_fifo();
			} else
		#elif defined(S_IFMT) && defined(S_IFIFO)
			if ((s.st_mode & S_IFMT) == S_IFIFO) {
				type = ML_file_type_fifo();
			} else
		#endif

		#if defined(S_ISLNK)
			if (S_ISLNK(s.st_mode)) {
				type = ML_file_type_symbolic_link();
			} else
		#elif defined(S_IFMT) && defined(S_IFLNK)
			if ((s.st_mode & S_IFMT) == S_IFLNK) {
				type = ML_file_type_symbolic_link();
			} else
		#endif

		#if defined(S_ISSOCK)
			if (S_ISSOCK(s.st_mode)) {
				type = ML_file_type_socket();
			} else
		#elif defined(S_IFMT) && defined(S_IFSOCK)
			if ((s.st_mode & S_IFMT) == S_IFSOCK) {
				type = ML_file_type_socket();
			} else
		#endif

		#ifdef S_TYPEISMQ
			if (S_TYPEISMQ(&s)) {
				type = ML_file_type_message_queue();
			} else
		#endif

		#ifdef S_TYPEISSEM
			if (S_TYPEISSEM(&s)) {
				type = ML_file_type_semaphore();
			} else
		#endif

		#ifdef S_TYPEISSHM
			if (S_TYPEISSHM(&s)) {
				type = ML_file_type_shared_memory();
			} else
		#endif

			{
				type = ML_file_type_unknown();
			}

		Result = ML_make_io_res_1_ok_file_type(type);
	} else {
		/*
		** We can't just call ML_make_err_msg here because
		** it uses `hp' and this procedure can call Mercury.
		*/
		ML_make_io_res_1_error_file_type(errno,
			MR_make_string_const(""io.file_type failed: ""),
			&Result);
	}
#else
	MR_fatal_error(
		""Sorry, io.file_type not implemented on this platform"") }
#endif
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__file_type_2(_FollowSymLinks::in, FileName::in,
		Result::out, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
	System.IO.FileAttributes attrs =
		System.IO.File.GetAttributes(FileName);
	if ((attrs & System.IO.FileAttributes.Directory) ==
			System.IO.FileAttributes.Directory)
	{
	    Result = mercury.io.mercury_code.ML_make_io_res_1_ok_file_type(
			mercury.io.mercury_code.ML_file_type_directory());
	}
	else if ((attrs & System.IO.FileAttributes.Device) ==
			System.IO.FileAttributes.Device)
	{
	    // XXX It may be a block device, but .NET doesn't
	    // distinguish between character and block devices.
	    Result = mercury.io.mercury_code.ML_make_io_res_1_ok_file_type(
		mercury.io.mercury_code.ML_file_type_character_device());
	}
	else
	{
	    Result = mercury.io.mercury_code.ML_make_io_res_1_ok_file_type(
		mercury.io.mercury_code.ML_file_type_regular());
	}
    } catch (System.Exception e) {
	    mercury.io.mercury_code.ML_make_io_res_1_error_file_type(e,
		""can't find file type: "", ref Result);
    }
").

:- func file_type_character_device = file_type.
:- func file_type_block_device = file_type.
:- func file_type_fifo = file_type.
:- func file_type_directory  = file_type.
:- func file_type_socket  = file_type.
:- func file_type_symbolic_link = file_type.
:- func file_type_regular = file_type.
:- func file_type_message_queue = file_type.
:- func file_type_semaphore = file_type.
:- func file_type_shared_memory = file_type.
:- func file_type_unknown = file_type.

file_type_character_device = character_device.
file_type_block_device = block_device.
file_type_fifo = named_pipe.
file_type_directory = directory.
file_type_socket = socket.
file_type_symbolic_link = symbolic_link.
file_type_regular = regular_file.
file_type_message_queue = message_queue.
file_type_semaphore = semaphore.
file_type_shared_memory = shared_memory.
file_type_unknown = unknown.

:- pragma export(file_type_character_device = out,
			"ML_file_type_character_device").
:- pragma export(file_type_block_device = out, "ML_file_type_block_device").
:- pragma export(file_type_fifo = out, "ML_file_type_fifo").
:- pragma export(file_type_directory = out, "ML_file_type_directory").
:- pragma export(file_type_socket = out, "ML_file_type_socket").
:- pragma export(file_type_symbolic_link = out, "ML_file_type_symbolic_link").
:- pragma export(file_type_regular = out, "ML_file_type_regular").
:- pragma export(file_type_message_queue = out, "ML_file_type_message_queue").
:- pragma export(file_type_semaphore = out, "ML_file_type_semaphore").
:- pragma export(file_type_shared_memory = out, "ML_file_type_shared_memory").
:- pragma export(file_type_unknown = out, "ML_file_type_unknown ").

%-----------------------------------------------------------------------------%

io__check_file_accessibility(FileName, AccessTypes, Result) -->
	( { have_dotnet } ->
		io__check_file_accessibility_dotnet(FileName, AccessTypes,
			Result)
	;
		io__check_file_accessibility_2(FileName, AccessTypes, Result)
	).

:- pred io__check_file_accessibility_2(string, list(access_type),
		io__res, io__state, io__state).
:- mode io__check_file_accessibility_2(in, in, out, di, uo) is det.

:- pragma foreign_proc("C",
	io__check_file_accessibility_2(FileName::in, AccessTypes::in,
		Result::out, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#if defined(MR_HAVE_ACCESS)
  #ifdef F_OK
	int mode = F_OK;
  #else 
	int mode = 0;
  #endif
	int access_result;

	if (ML_access_types_includes_execute(AccessTypes)) {
		#ifdef X_OK
			mode |= X_OK;
		#else
			mode |= 1;
		#endif
	}
	if (ML_access_types_includes_write(AccessTypes)) {
		#ifdef W_OK
			mode |= W_OK;
		#else
			mode |= 2;
		#endif
	}
	if (ML_access_types_includes_read(AccessTypes)) {
		#ifdef R_OK
			mode |= R_OK;
		#else
			mode |= 4;
		#endif
	}

	access_result = access(FileName, mode);
	if (access_result == 0) {
		Result = ML_make_io_res_0_ok();
	} else {
		ML_make_io_res_0_error(errno,
			MR_make_string_const(
				""file not accessible: ""),
			&Result);
	}
#else /* !MR_HAVE_ACCESS */
	Result = ML_make_io_res_0_error_msg(
	""io.check_file_accessibility not supported on this platform"");
#endif
	IO = IO0;
}").

	% The .NET CLI doesn't provide an equivalent of access(), so
	% we have to try to open the file to see if it is accessible.
:- pred io__check_file_accessibility_dotnet(string::in, list(access_type)::in,
		io__res::out, io__state::di, io__state::uo) is det.

io__check_file_accessibility_dotnet(FileName, AccessTypes, Result, !IO) :-
	CheckRead0 = pred_to_bool(access_types_includes_read(AccessTypes)),
	CheckWrite = pred_to_bool(access_types_includes_write(AccessTypes)),

	CheckExec = pred_to_bool(access_types_includes_execute(AccessTypes)),
	% We need to be able to read a file to execute it.
	CheckRead = bool__or(CheckRead0, CheckExec),

	io__file_type(yes, FileName, FileTypeRes, !IO),
	(
		FileTypeRes = ok(FileType),
		( FileType = directory ->
			check_directory_accessibility_dotnet(FileName,
				to_int(CheckRead), to_int(CheckWrite),
				Result, !IO)
		;
			( CheckRead = yes ->
				io__open_input(FileName, InputRes, !IO),
				(
					InputRes = ok(InputStream),
					io__close_input(InputStream, !IO),
					CheckReadRes = ok
				;
					InputRes = error(InputError),
					CheckReadRes = error(InputError)
				)
			;
				CheckReadRes = ok
			),
			( CheckReadRes = ok, CheckWrite = yes ->
				io__open_append(FileName, OutputRes, !IO),
				(
					OutputRes = ok(OutputStream),
					io__close_output(OutputStream, !IO),
					CheckWriteRes = ok
				;
					OutputRes = error(OutputError),
					CheckWriteRes = error(OutputError)
				)
			;
				CheckWriteRes = CheckReadRes
			),
			(
				CheckWriteRes = ok,

				% Unix programs need to check whether the
				% execute bit is set for the directory, but
				% we can't actually execute the directory.
				CheckExec = yes
			->
				have_dotnet_exec_permission(Result, !IO)
			;
				Result = CheckWriteRes
			)
		)
	;
		FileTypeRes = error(FileTypeError),
		Result = error(FileTypeError)
	).

:- pred have_dotnet_exec_permission(io__res, io__state, io__state).
:- mode have_dotnet_exec_permission(out, di, uo) is det.

have_dotnet_exec_permission(_, !IO) :-
	error(
	"io.have_dotnet_exec_permission invoked for non-.NET CLI backend").

:- pragma foreign_proc("C#",
	have_dotnet_exec_permission(Result::out, _IO0::di, _IO::uo),
	[promise_pure, may_call_mercury, thread_safe],
"{
    try {
        // We need unrestricted permissions to execute
        // unmanaged code.
        (new System.Security.Permissions.SecurityPermission(
            System.Security.Permissions.SecurityPermissionFlag.AllFlags)).
            Demand();
        Result = mercury.io.mercury_code.ML_make_io_res_0_ok();
    } catch (System.Exception e) {
        mercury.io.mercury_code.ML_make_io_res_0_error(e,
            ""execute permission check failed: "", ref Result);
    }

}").

:- pred check_directory_accessibility_dotnet(string::in, int::in, int::in,
		io__res::out, io__state::di, io__state::uo) is det.

check_directory_accessibility_dotnet(_, _, _, _, _, _) :-
	error(
"io.check_directory_accessibility_dotnet called for non-.NET CLI backend").

:- pragma foreign_proc("C#",
	check_directory_accessibility_dotnet(FileName::in, CheckRead::in,
		CheckWrite::in, Result::out, _IO0::di, _IO::uo),
	[promise_pure, may_call_mercury, tabled_for_io, thread_safe],
"{
	try {
		if (CheckRead != 0) {
			// XXX This is less efficient than I would like.
			// Unfortunately the .NET CLI has no function
			// corresponding to access() or opendir().
			System.IO.Directory.GetFileSystemEntries(FileName);
		}
		if (CheckWrite != 0) {
			// This will fail if the .NET CLI security regime
			// we're operating under doesn't allow writing
			// to the file. Even if this succeeds, the file
			// system may disallow write access.
			System.IO.Directory.SetLastAccessTime(FileName,
				System.DateTime.Now);

			// XXX This isn't quite right.
			// Just because the directory isn't read-only
			// doesn't mean we have permission to write to it.
			// The only way to test whether a directory is
			// writable is to write a file to it.
			// The ideal way to do that would be io__make_temp,
			// but currently the .NET backend version of that
			// ignores the directory passed to it.
			System.IO.FileAttributes attrs =
				System.IO.File.GetAttributes(FileName);
			if ((attrs & System.IO.FileAttributes.ReadOnly) ==
				System.IO.FileAttributes.ReadOnly)
			{
				throw (new
				    System.Exception(""file is read-only""));
			}
		}
		Result = mercury.io.mercury_code.ML_make_io_res_0_ok();
	} catch (System.Exception e) {
		mercury.io.mercury_code.ML_make_io_res_0_error(e,
			""permission check failed: "", ref Result);
	}
}").

:- pred access_types_includes_read(list(access_type)::in) is semidet.
:- pragma export(access_types_includes_read(in),
		"ML_access_types_includes_read").
access_types_includes_read(Access) :- list__member(read, Access).

:- pred access_types_includes_write(list(access_type)::in) is semidet.
:- pragma export(access_types_includes_write(in),
		"ML_access_types_includes_write").
access_types_includes_write(Access) :- list__member(write, Access).

:- pred access_types_includes_execute(list(access_type)::in) is semidet.
:- pragma export(access_types_includes_execute(in),
		"ML_access_types_includes_execute").
access_types_includes_execute(Access) :- list__member(execute, Access).

:- func make_io_res_0_ok = io__res.
:- pragma export((make_io_res_0_ok = out), "ML_make_io_res_0_ok").
make_io_res_0_ok = ok.

:- pred make_io_res_0_error(io__system_error::in, string::in, io__res::out,
		io__state::di, io__state::uo) is det.
:- pragma export(make_io_res_0_error(in, in, out, di, uo),
		"ML_make_io_res_0_error").
make_io_res_0_error(Error, Msg0, error(make_io_error(Msg))) -->
	io__make_err_msg(Error, Msg0, Msg).

:- func make_io_res_0_error_msg(string) = io__res.
:- pragma export((make_io_res_0_error_msg(in) = out),
		"ML_make_io_res_0_error_msg").
make_io_res_0_error_msg(Msg) = error(make_io_error(Msg)).

:- func make_io_res_1_ok_file_type(file_type) = io__res(file_type).
:- pragma export((make_io_res_1_ok_file_type(in) = out),
		"ML_make_io_res_1_ok_file_type").
make_io_res_1_ok_file_type(FileType) = ok(FileType).

:- pred make_io_res_1_error_file_type(io__system_error::in,
		string::in, io__res(file_type)::out,
		io__state::di, io__state::uo) is det.
:- pragma export(make_io_res_1_error_file_type(in, in, out, di, uo),
		"ML_make_io_res_1_error_file_type").
make_io_res_1_error_file_type(Error, Msg0, error(make_io_error(Msg))) -->
	io__make_err_msg(Error, Msg0, Msg).

%-----------------------------------------------------------------------------%

:- type file_id ---> file_id.
:- pragma foreign_type("C", file_id, "ML_File_Id") where
		comparison is compare_file_id.

:- pragma foreign_decl("C",
"
#ifdef MR_HAVE_DEV_T
  typedef	dev_t		ML_dev_t;
#else
  typedef	MR_Integer	ML_dev_t;
#endif

#ifdef MR_HAVE_INO_T
  typedef	ino_t		ML_ino_t;
#else
  typedef	MR_Integer	ML_ino_t;
#endif

typedef struct {
  	ML_dev_t device;
  	ML_ino_t inode;
} ML_File_Id;
").

:- pred compare_file_id(comparison_result::uo,
		file_id::in, file_id::in) is det.

compare_file_id(Result, FileId1, FileId2) :-
	compare_file_id_2(Result0, FileId1, FileId2),
	Result =
	    ( if Result0 < 0 then (<) else if Result0 = 0 then (=) else (>) ).

:- pred compare_file_id_2(int::out, file_id::in, file_id::in) is det.

:- pragma foreign_proc("C",
	compare_file_id_2(Res::out, FileId1::in, FileId2::in),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	int device_cmp;
	int inode_cmp;

	/*
	** For compilers other than GCC, glibc defines dev_t as
	** struct (dev_t is 64 bits, and other compilers may
	** not have a 64 bit arithmetic type).
	** XXX This code assumes that dev_t and ino_t do not include
	** padding bits.  In practice, that should be OK.
	*/
	device_cmp = memcmp(&(FileId1.device), &(FileId2.device),
			sizeof(ML_dev_t));

	if (device_cmp < 0) {
		Res = -1;
	} else if (device_cmp > 0) {
		Res = 1;
	} else {
		inode_cmp = memcmp(&(FileId1.inode), &(FileId2.inode),
				sizeof(ML_ino_t));
		if (device_cmp < 0) {
			Res = -1;
		} else if (device_cmp > 0) {
			Res = 1;
		} else {	
			Res = 0;
		}
	}
").

io__file_id(FileName, Result) -->
	( { have_file_ids } ->
		io__file_id_2(FileName, Status, Msg, FileId),
		( { Status = 1 } ->
			{ Result = ok(FileId) }
		;
			{ Result = error(io_error(Msg)) }
		)
	;
		{ Result = error(
	make_io_error("io.file_id not implemented on this platform")) }
	).

:- pred io__file_id_2(string, int, string, file_id, io__state, io__state).
:- mode io__file_id_2(in, out, out, out, di, uo) is det.

:- pragma foreign_proc("C",
	io__file_id_2(FileName::in, Status::out, Msg::out,
		FileId::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_STAT
	struct stat s;
	if (stat(FileName, &s) == 0) {
		FileId.device = s.st_dev;
		FileId.inode = s.st_ino;
		Msg = MR_string_const("""", 0);
		Status = 1;
	} else {
		ML_maybe_make_err_msg(MR_TRUE, errno, ""stat() failed: "",
			MR_PROC_LABEL, Msg);
		Status = 0;
	}
	MR_update_io(IO0, IO);
#else
	MR_fatal_error(""io.file_id_2 called but not supported"");
#endif
}").

% Can we retrieve inode numbers on this system.
have_file_ids :- semidet_fail.
:- pragma foreign_proc("C", have_file_ids,
	[promise_pure, will_not_call_mercury, thread_safe],
"
#if defined(MR_BROKEN_STAT_ST_INO) || !defined(MR_HAVE_STAT)
	/* Win32 returns junk in the st_ino field of `struct stat'. */
	SUCCESS_INDICATOR = MR_FALSE;
#else
	SUCCESS_INDICATOR = MR_TRUE;
#endif
").

%-----------------------------------------------------------------------------%

% A `buffer' is just an array of Chars.
% Buffer sizes are measured in Chars.

:- type buffer.
:- pragma foreign_type(c, buffer, "MR_Char *").

	% XXX It would be better to use a char_array (e.g. defined as char[] in
	% C#) type rather than array(char).  This is because on the Java and IL
	% backends indexing into an array whose element type is known
	% statically requires less overhead.
:- type buffer ---> buffer(array(char)).

	% XXX Extend the workaround for no `ui' modes in array.m.
:- inst uniq_buffer = bound(buffer(uniq_array)).
:- mode buffer_di == di(uniq_buffer).
:- mode buffer_uo == out(uniq_buffer).

:- pred io__alloc_buffer(int::in, buffer::buffer_uo) is det.
:- pragma foreign_proc("C", 
	io__alloc_buffer(Size::in, Buffer::buffer_uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MR_Word buf;
	MR_offset_incr_hp_atomic_msg(buf, 0,
		(Size * sizeof(MR_Char) + sizeof(MR_Word) - 1)
			/ sizeof(MR_Word),
		MR_PROC_LABEL, ""io:buffer/0"");
	Buffer = (MR_Char *) buf;
}").

io__alloc_buffer(Size, buffer(Array)) :-
		% XXX '0' is used as Mercury doesn't recognise '\0' as 
		% a char constant.
	array__init(Size, '0', Array).

:- pred io__resize_buffer(buffer::buffer_di, int::in, int::in,
		buffer::buffer_uo) is det.
:- pragma foreign_proc("C",
	io__resize_buffer(Buffer0::buffer_di, OldSize::in,
		NewSize::in, Buffer::buffer_uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	MR_CHECK_EXPR_TYPE(Buffer0, MR_Char *);
	MR_CHECK_EXPR_TYPE(Buffer, MR_Char *);

#ifdef MR_CONSERVATIVE_GC
	Buffer = MR_GC_realloc(Buffer0, NewSize * sizeof(MR_Char));
#else
	if (Buffer0 + OldSize == (MR_Char *) MR_hp) {
		MR_Word next;
		MR_offset_incr_hp_atomic_msg(next, 0,
		   (NewSize * sizeof(MR_Char) + sizeof(MR_Word) - 1)
		   	/ sizeof(MR_Word),
		   MR_PROC_LABEL, ""io:buffer/0"");
		assert(Buffer0 + OldSize == (MR_Char *) next);
	    	Buffer = Buffer0;
	} else {
		/* just have to alloc and copy */
		MR_Word buf;
		MR_offset_incr_hp_atomic_msg(buf, 0,
		   (NewSize * sizeof(MR_Char) + sizeof(MR_Word) - 1)
		   	/ sizeof(MR_Word),
		   MR_PROC_LABEL, ""io:buffer/0"");
		Buffer = (MR_Char *) buf;
		if (OldSize > NewSize) {
			memcpy(Buffer, Buffer0, NewSize);
		} else {
			memcpy(Buffer, Buffer0, OldSize);
		}
	}
#endif
}").

io__resize_buffer(buffer(Array0), _OldSize, NewSize, buffer(Array)) :-
		% XXX '0' is used as Mercury doesn't recognise '\0' as 
		% a char constant.
	array__resize(Array0, NewSize, '0', Array).

:- pred io__buffer_to_string(buffer::buffer_di, int::in, string::uo) is det.
:- pragma foreign_proc("C", 
	io__buffer_to_string(Buffer::buffer_di, Len::in, Str::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	Str = Buffer;
	Str[Len] = '\\0';
}").

io__buffer_to_string(buffer(Array), Len, from_char_list(List)) :-
	array__fetch_items(Array, min(Array), min(Array) + Len - 1, List).

:- pred io__read_into_buffer(stream::in, buffer::buffer_di, int::in, int::in,
		    buffer::buffer_uo, int::out,
		    io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__read_into_buffer(Stream::in, Buffer0::buffer_di, Pos0::in,
		    Size::in, Buffer::buffer_uo, Pos::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	int		items_read;

	MR_CHECK_EXPR_TYPE(Buffer0, MR_Char *);
	MR_CHECK_EXPR_TYPE(Buffer, MR_Char *);

	items_read = MR_READ(*Stream, Buffer0 + Pos0, Size - Pos0);

	Buffer = Buffer0;
	Pos = Pos0 + items_read;
	MR_update_io(IO0, IO);
}").

io__read_into_buffer(Stream, buffer(Array0), Pos0, Size, buffer(Array), Pos) -->
	io__read_into_array(Stream, Array0, Pos0, Size, Array, Pos).

:- pred io__read_into_array(stream::in, array(char)::array_di, int::in, int::in,
		    array(char)::array_uo, int::out,
		    io__state::di, io__state::uo) is det.

io__read_into_array(Stream, Array0, Pos0, Size, Array, Pos) -->
	( { Pos0 >= Size } ->
		{ Array = Array0 },
		{ Pos = Pos0 }
	;
		io__read_char(Stream, CharResult),
		( { CharResult = ok(Char) } ->
			{ array__set(Array0, Pos0, Char, Array1) },
			io__read_into_array(Stream, Array1, Pos0 + 1,
					Size, Array, Pos)
		;
			{ Array = Array0 },
			{ Pos = Pos0}
		)
	).

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

%-----------------------------------------------------------------------------%

io__binary_input_stream_foldl(Pred, T0, Res) -->
	io__binary_input_stream(Stream),
	io__binary_input_stream_foldl(Stream, Pred, T0, Res).

io__binary_input_stream_foldl(Stream, Pred, T0, Res) -->
	io__read_byte(Stream, ByteResult),
	(
		{ ByteResult = ok(Byte) },
		{ Pred(Byte, T0, T1) },
		io__binary_input_stream_foldl(Stream, Pred, T1, Res)
	;
		{ ByteResult = eof },
		{ Res = ok(T0) }
	;
		{ ByteResult = error(Error) },
		{ Res = error(T0, Error) }
	).

io__binary_input_stream_foldl_io(Pred, Res) -->
	io__binary_input_stream(Stream),
	io__binary_input_stream_foldl_io(Stream, Pred, Res).

io__binary_input_stream_foldl_io(Stream, Pred, Res) -->
	io__read_byte(Stream, ByteResult),
	(
		{ ByteResult = ok(Byte) },
		Pred(Byte),
		io__binary_input_stream_foldl_io(Stream, Pred, Res)
	;
		{ ByteResult = eof },
		{ Res = ok }
	;
		{ ByteResult = error(Error) },
		{ Res = error(Error) }
	).

io__binary_input_stream_foldl2_io(Pred, T0, Res) -->
	io__binary_input_stream(Stream),
	io__binary_input_stream_foldl2_io(Stream, Pred, T0, Res).

io__binary_input_stream_foldl2_io(Stream, Pred, T0, Res) -->
	io__read_byte(Stream, ByteResult),
	(
		{ ByteResult = ok(Byte) },
		Pred(Byte, T0, T1),
		io__binary_input_stream_foldl2_io(Stream, Pred, T1, Res)
	;
		{ ByteResult = eof },
		{ Res = ok(T0) }
	;
		{ ByteResult = error(Error) },
		{ Res = error(T0, Error) }
	).

io__binary_input_stream_foldl2_io_maybe_stop(Pred, T0, Res) -->
	io__binary_input_stream(Stream),
	io__binary_input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res).

io__binary_input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res) -->
	io__read_byte(Stream, ByteResult),
	(
		{ ByteResult = ok(Byte) },
		Pred(Byte, Continue, T0, T1),
		(
			{ Continue = no },
			{ Res = ok(T1) }
		;
			{ Continue = yes },
			io__binary_input_stream_foldl2_io_maybe_stop(Stream,
				Pred, T1, Res)
		)
	;
		{ ByteResult = eof },
		{ Res = ok(T0) }
	;
		{ ByteResult = error(Error) },
		{ Res = error(T0, Error) }
	).

%-----------------------------------------------------------------------------%

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
	; { univ_to_type(Univ, Stream) } ->
		io__get_stream_db(StreamDb),
		{ io__maybe_stream_info(StreamDb, Stream) = StreamInfo },
		{ type_to_univ(StreamInfo, StreamInfoUniv) },
		io__do_write_univ(NonCanon, StreamInfoUniv, Priority)
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
		io__format("%s.%s/%d", [s(ModuleName), s(Name), i(Arity)])
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
	io__do_open_text(FileName, "r", Result0, OpenCount, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_info(NewStream,
			stream(OpenCount, input, text, file(FileName)))
	;
		io__make_err_msg("can't open input file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_output(FileName, Result) -->
	io__do_open_text(FileName, "w", Result0, OpenCount, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_info(NewStream,
			stream(OpenCount, output, text, file(FileName)))
	;
		io__make_err_msg("can't open output file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_append(FileName, Result) -->
	io__do_open_text(FileName, "a", Result0, OpenCount, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_info(NewStream,
			stream(OpenCount, append, text, file(FileName)))
	;
		io__make_err_msg("can't append to file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_binary_input(FileName, Result) -->
	io__do_open_binary(FileName, "rb", Result0, OpenCount, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_info(NewStream,
			stream(OpenCount, input, binary, file(FileName)))
	;
		io__make_err_msg("can't open input file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_binary_output(FileName, Result) -->
	io__do_open_binary(FileName, "wb", Result0, OpenCount, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_info(NewStream,
			stream(OpenCount, output, binary, file(FileName)))
	;
		io__make_err_msg("can't open output file: ", Msg),
		{ Result = error(io_error(Msg)) }
	).

io__open_binary_append(FileName, Result) -->
	io__do_open_binary(FileName, "ab", Result0, OpenCount, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_info(NewStream,
			stream(OpenCount, append, binary, file(FileName)))
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
	io__stream_info(Stream, MaybeInfo),
	{
		MaybeInfo = yes(Info),
		Info = stream(_, _, _, Source),
		Name = source_name(Source)
	;
		MaybeInfo = no,
		Name = "<stream name unavailable>"
	}.

:- pred io__stream_info(io__stream::in, maybe(stream_info)::out,
	io__state::di, io__state::uo) is det.

io__stream_info(Stream, MaybeInfo) -->
	io__get_stream_db(StreamDb),
	{ map__search(StreamDb, get_stream_id(Stream), Info) ->
		MaybeInfo = yes(Info)
	;
		MaybeInfo = no
	}.

io__input_stream_info(StreamDb, Stream) =
	io__maybe_stream_info(StreamDb, Stream).

io__output_stream_info(StreamDb, Stream) =
	io__maybe_stream_info(StreamDb, Stream).

io__binary_input_stream_info(StreamDb, Stream) =
	io__maybe_stream_info(StreamDb, Stream).

io__binary_output_stream_info(StreamDb, Stream) =
	io__maybe_stream_info(StreamDb, Stream).

:- func io__maybe_stream_info(io__stream_db, io__stream) = maybe_stream_info.

io__maybe_stream_info(StreamDb, Stream) = Info :-
	( map__search(StreamDb, get_stream_id(Stream), Info0) ->
		Info0 = stream(Id, Mode, Content, Source),
		Info  = stream(Id, Mode, Content, Source)
	;
		Info  = unknown_stream
	).

:- func maybe_source_name(maybe(stream_info)) = string.

maybe_source_name(MaybeInfo) = Name :-
	(
		MaybeInfo = yes(Info),
		Info = stream(_, _, _, Source),
		Name = source_name(Source)
	;
		MaybeInfo = no,
		Name = "<stream name unavailable>"
	).

:- func source_name(stream_source) = string.

source_name(file(Name)) = Name.
source_name(stdin) = "<standard input>".
source_name(stdout) = "<standard output>".
source_name(stderr) = "<standard error>".

:- pragma foreign_proc("C", 
	io__get_stream_db(StreamDb::out, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	StreamDb = ML_io_stream_db;
	MR_update_io(IO0, IO);
").

:- pred io__set_stream_db(io__stream_db::in, io__state::di, io__state::uo)
	is det.

:- pragma foreign_proc("C", 
	io__set_stream_db(StreamDb::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ML_io_stream_db = StreamDb;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#", 
	io__get_stream_db(StreamDb::out, _IO0::di, _IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	StreamDb = ML_io_stream_db;
").

:- pragma foreign_proc("C#", 
	io__set_stream_db(StreamDb::in, _IO0::di, _IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ML_io_stream_db = StreamDb;
").

%-----------------------------------------------------------------------------%

:- pred io__insert_stream_info(io__stream::in, stream_info::in,
	io__state::di, io__state::uo) is det.

io__insert_stream_info(Stream, Name) -->
	io__get_stream_db(StreamDb0),
	{ map__set(StreamDb0, get_stream_id(Stream), Name, StreamDb) },
	io__set_stream_db(StreamDb).

:- pred io__maybe_delete_stream_info(io__stream::in,
	io__state::di, io__state::uo) is det.

io__maybe_delete_stream_info(Stream) -->
	io__may_delete_stream_info(MayDeleteStreamInfo),
	( { MayDeleteStreamInfo \= 0 } ->
		io__get_stream_db(StreamDb0),
		{ map__delete(StreamDb0, get_stream_id(Stream), StreamDb) },
		io__set_stream_db(StreamDb)
	;
		[]
	).

% Return an integer that is nonzero if and only if we should delete
% the information we have about stream when that stream is closed.
% The debugger may need this information in order to display the stream id
% in a user-friendly manner even after the stream is closed (e.g. after
% performing a retry after the close), so if debugging is enabled, we
% hang on to the stream info until the end of the execution. This is a
% space leak, but one that is acceptable in a program being debugged.

:- pred io__may_delete_stream_info(int::out,
	io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__may_delete_stream_info(MayDelete::out, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	MayDelete = !MR_trace_ever_enabled;
	IO = IO0;
").

io__may_delete_stream_info(1, !IO).

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
	MR_update_io(IOState0, IOState);
").

:- pragma foreign_proc("C", 
	io__set_globals(Globals::di, IOState0::di, IOState::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* XXX need to globalize the memory */
	ML_io_user_globals = Globals;
	MR_update_io(IOState0, IOState);
").

:- pragma foreign_proc("C#", 
	io__get_globals(Globals::uo, _IOState0::di, _IOState::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Globals = ML_io_user_globals;
").

:- pragma foreign_proc("C#", 
	io__set_globals(Globals::di, _IOState0::di, _IOState::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ML_io_user_globals = Globals;
").

io__progname_base(DefaultName, PrognameBase) -->
	io__progname(DefaultName, Progname),
	{ PrognameBase = dir__basename_det(Progname) }.


	% XXX we call a pred version of io__get_stream_id, which is a
	% bit inelegant.  We should either fix the MC++ interface so you
	% can implement functions, or implement everything in this
	% module in C#.

io__get_stream_id(Stream) = Id :- io__get_stream_id(Stream, Id).

:- pred io__get_stream_id(io__stream::in, io__stream_id::out) is det.

:- pragma foreign_proc("C",
	io__get_stream_id(Stream::in, Id::out), 
	[will_not_call_mercury, promise_pure],
"

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
	Id = (Stream)->id;
#endif
").

:- pragma foreign_proc("C#",
	io__get_stream_id(Stream::in, Id::out), 
	[will_not_call_mercury, promise_pure],
"
	Id = Stream.id;
").

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
	( { impure io__setenv(Var, Value) } ->
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
	io__gc_init(type_of(StreamDb), type_of(Globals)),
	{ map__init(StreamDb) },
	{ type_to_univ("<globals>", Globals) },
	io__set_stream_db(StreamDb),
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
	io__gc_init(StreamDbType::in, UserGlobalsType::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	/* for Windows DLLs, we need to call GC_INIT() from each DLL */
#ifdef MR_CONSERVATIVE_GC
	GC_INIT();
#endif
	MR_add_root(&ML_io_stream_db, (MR_TypeInfo) StreamDbType);
	MR_add_root(&ML_io_user_globals, (MR_TypeInfo) UserGlobalsType);
	MR_update_io(IO0, IO);
").

io__gc_init(_, _) --> [].

:- pred io__insert_std_stream_names(io__state, io__state).
:- mode io__insert_std_stream_names(di, uo) is det.

io__insert_std_stream_names -->
	io__stdin_stream(Stdin),
	io__insert_stream_info(Stdin, stream(0, input, preopen, stdin)),
	io__stdout_stream(Stdout),
	io__insert_stream_info(Stdout, stream(1, output, preopen, stdout)),
	io__stderr_stream(Stderr),
	io__insert_stream_info(Stderr, stream(1, output, preopen, stderr)).

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

io__make_io_error(Error) = io_error(Error).

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
#include <limits.h>

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

#define MR_initial_io_state()		0	/* some random number */
#define MR_final_io_state(r)		((void)0)

#define MR_update_io(r_src, r_dest)	((r_dest) = (r_src))

void 		mercury_init_io(void);
MercuryFilePtr	mercury_open(const char *filename, const char *openmode);
void		mercury_io_error(MercuryFilePtr mf, const char *format, ...);
void		mercury_output_error(MercuryFilePtr mf);
void		mercury_print_string(MercuryFilePtr mf, const char *s);
void		mercury_print_binary_string(MercuryFilePtr mf, const char *s);
int		mercury_getc(MercuryFilePtr mf);
void		mercury_close(MercuryFilePtr mf);
int		ML_fprintf(MercuryFilePtr mf, const char *format, ...);
").


:- pragma foreign_decl("C#", "

namespace mercury {
  namespace io__csharp_code {
    public enum ML_file_encoding_kind {
	ML_OS_text_encoding,	// file stores characters,
				// using the operating system's
				// default encoding, and OS's
				// usual line-ending convention
				// (e.g. CR-LF for DOS/Windows).

	ML_Unix_text_encoding,	// file stores characters,
				// using the operating system's
				// default encoding, but with the
				// Unix line-ending convention.

	ML_raw_binary		// file stores bytes
    };

    public class MR_MercuryFileStruct {
	// Note that stream reader and writer are initialized lazily;
	// that is, if the stream has not yet been used for reading,
	// the `reader' field may be null.  Any code which accesses that
	// field must check for null and initialize it if needed.
	// Likewise for the `writer' field.

	public System.IO.Stream 	stream; // The stream itself
	public System.IO.TextReader 	reader; // The stream reader for it
	public System.IO.TextWriter 	writer; // The stream write for it
	public int			putback;
				// the next character or byte to read,
				// or -1 if no putback char/byte is stored

	public ML_file_encoding_kind	file_encoding;
				// DOS, Unix, or raw binary

	public int			line_number;
	public int			id;

    };
  }
}
").

:- pragma foreign_code("C", "

MercuryFile mercury_stdin;
MercuryFile mercury_stdout;
MercuryFile mercury_stderr;
MercuryFile mercury_stdin_binary;
MercuryFile mercury_stdout_binary;

MercuryFilePtr mercury_current_text_input = &mercury_stdin;
MercuryFilePtr mercury_current_text_output = &mercury_stdout;
MercuryFilePtr mercury_current_binary_input = &mercury_stdin_binary;
MercuryFilePtr mercury_current_binary_output = &mercury_stdout_binary;

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

:- pragma foreign_code("C#", "

static MR_MercuryFileStruct
mercury_file_init(System.IO.Stream stream,
	System.IO.TextReader reader, System.IO.TextWriter writer,
	ML_file_encoding_kind file_encoding)
{
	MR_MercuryFileStruct mf = new MR_MercuryFileStruct();
	mf.stream = stream;
	mf.reader = reader;
	mf.putback = -1;
	mf.writer = writer;
	mf.file_encoding = file_encoding;
	mf.line_number = 1;
	mf.id = ML_next_stream_id++;
	return mf;
}

	// Note: for Windows GUI programs, the Console is set to the
	// equivalent of /dev/null.  This could perhaps be considered a
	// problem.  But if so, it is a problem in Windows, not in Mercury --
	// I don't think it is one that the Mercury implementation should
	// try to solve.

static MR_MercuryFileStruct mercury_stdin =
	mercury_file_init(System.Console.OpenStandardInput(),
	    System.Console.In, null, ML_default_text_encoding);
static MR_MercuryFileStruct mercury_stdout =
	mercury_file_init(System.Console.OpenStandardOutput(),
	    null, System.Console.Out, ML_default_text_encoding);
static MR_MercuryFileStruct mercury_stderr =
	mercury_file_init(System.Console.OpenStandardError(),
	    null, System.Console.Error, ML_default_text_encoding);

	// XXX should we use BufferedStreams here?
static MR_MercuryFileStruct mercury_stdin_binary =
	mercury_file_init(System.Console.OpenStandardInput(),
	    System.Console.In, null, ML_file_encoding_kind.ML_raw_binary);
static MR_MercuryFileStruct mercury_stdout_binary =
	mercury_file_init(System.Console.OpenStandardOutput(),
	    null, System.Console.Out, ML_file_encoding_kind.ML_raw_binary);

static MR_MercuryFileStruct mercury_current_text_input = mercury_stdin;
static MR_MercuryFileStruct mercury_current_text_output = mercury_stdout;
static MR_MercuryFileStruct mercury_current_binary_input = mercury_stdin_binary;
static MR_MercuryFileStruct mercury_current_binary_output = mercury_stdout_binary;

// XXX not thread-safe! */
static System.Exception MR_io_exception;

").


:- pragma foreign_code("C", "

MercuryFilePtr
mercury_open(const char *filename, const char *openmode)
{
	MercuryFilePtr mf;
	FILE *f;

	f = fopen(filename, openmode);
	if (f == NULL) {
		return NULL;
	}
	mf = MR_GC_NEW(MercuryFile);
	MR_mercuryfile_init(f, 1, mf);
	return mf;
}

").

:- pragma foreign_code("C#", "

static MR_MercuryFileStruct mercury_open(string filename, string openmode,
	ML_file_encoding_kind file_encoding)
{
        System.IO.FileMode   mode;
        System.IO.FileAccess access;
        System.IO.FileShare  share;
        System.IO.Stream     stream = null;

	try {
		if (openmode == ""r"" || openmode == ""rb"") {
			// Like '<' in Bourne shell.
			// Read a file.  The file must exist already.
			mode   = System.IO.FileMode.Open;
			access = System.IO.FileAccess.Read;
		} else if (openmode == ""w"" || openmode == ""wb"") {
			// Like '>' in Bourne shell.
			// Overwrite an existing file, or create a new file.
			mode   = System.IO.FileMode.Create;
			access = System.IO.FileAccess.Write;
		} else if (openmode == ""a"" || openmode == ""ab"") {
			// Like '>>' in Bourne shell.
			// Append to an existing file, or create a new file.
			mode   = System.IO.FileMode.Append;
			access = System.IO.FileAccess.Write;
		} else {
			mercury.runtime.Errors.SORRY(System.String.Concat(
				""foreign code for this function, open mode:"",
				openmode));
			// Needed to convince the C# compiler that mode and
			// access are always initialized.
			throw new System.Exception();
		}

		// For Unix compatibility, we allow files
		// to be read or written by multiple processes
		// simultaneously.  XXX Is this a good idea?
		share = System.IO.FileShare.ReadWrite;

		stream = System.IO.File.Open(filename, mode, access, share);

	} catch (System.Exception e) {
		MR_io_exception = e;
	}

        if (stream == null) {
                return null;
        } else {
		// we initialize the `reader' and `writer' fields to null;
		// they will be filled in later if they are needed.
                return mercury_file_init(
			new System.IO.BufferedStream(stream),
			null, null, file_encoding);
        }
}

").


:- pred throw_io_error(string::in) is erroneous.
:- pragma export(throw_io_error(in), "ML_throw_io_error").
throw_io_error(Message) :- throw(io_error(Message)).

:- pragma foreign_code("C", "

void
mercury_io_error(MercuryFilePtr mf, const char *format, ...)
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
mercury_output_error(MercuryFilePtr mf)
{
	mercury_io_error(mf, ""error writing to output file: %s"",
		strerror(errno));
}

").

:- pragma foreign_code("C", "

void
mercury_print_string(MercuryFilePtr mf, const char *s)
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

:- pragma foreign_code("C#", "

// Any changes here should also be reflected in the code for
// io__write_char, which (for efficiency) uses its own inline
// code, rather than calling this function.
static void
mercury_print_string(MR_MercuryFileStruct mf, string s)
{
	//
	// For the .NET back-end, strings are represented as Unicode.
	// Text output streams (which may be connected to text files,
	// or to the console) require a byte stream.
	// This raises the question: how should we
	// convert from Unicode to the byte sequence?
	//
	// We leave this up to the system, by just using the TextWriter
	// associated with the file.  For the console, this will be
	// System.Console.Out, which will use whatever encoding
	// is appropriate for the console.  For a file, the TextWriter
	// will use the System.Encoding.Default encoding, which
	// will normally be an 8-bit national character set.
	// If the Unicode string contains characters which can't be
	// represented in this set, then the encoder will throw an exception.
	//
	// For files, we construct the TextWriter here, rather than at file
	// open time, so that we don't try to construct TextWriters for
	// input streams.

	if (mf.writer == null) {
		mf.writer = new System.IO.StreamWriter(mf.stream,
			System.Text.Encoding.Default);
		
	}

	switch (mf.file_encoding) {
	case ML_file_encoding_kind.ML_raw_binary:
	case ML_file_encoding_kind.ML_Unix_text_encoding:
		mf.writer.Write(s);
		for (int i = 0; i < s.Length; i++) {
			if (s[i] == '\\n') {
				mf.line_number++;
			}
		}
		break;
	case ML_file_encoding_kind.ML_OS_text_encoding:
		//
		// We can't just use the System.TextWriter.Write(String)
		// method, since that method doesn't convert newline
		// characters to the system's newline convention
		// (e.g. CR-LF on Windows).
		// Only the WriteLine(...) method handles those properly.
		// So we have to output each character separately.
		//
		for (int i = 0; i < s.Length; i++) {
			if (s[i] == '\\n') {
				mf.line_number++;
				mf.writer.WriteLine("""");
			} else {
				mf.writer.Write(s[i]);
			}
		}
		break;
	}
}

").

:- pragma foreign_code("C", "

void
mercury_print_binary_string(MercuryFilePtr mf, const char *s)
{
	if (ML_fprintf(mf, ""%s"", s) < 0) {
		mercury_output_error(mf);
	}
}

").

:- pragma foreign_code("C", "

int
mercury_getc(MercuryFilePtr mf)
{
	int c = MR_GETCH(*mf);
	if (c == '\\n') {
		MR_line_number(*mf)++;
	}
	return c;
}

").


:- pragma foreign_code("C#", "

static void
mercury_print_binary_string(MR_MercuryFileStruct mf, string s)
{
	// sanity check
	if (mf.file_encoding != ML_file_encoding_kind.ML_raw_binary) {
		mercury.runtime.Errors.fatal_error(
	""mercury_print_binary_string: file encoding is not raw binary"");
	}

	//
	// For the .NET back-end, strings are represented as Unicode.
	// Binary files are stored as byte sequences.  This raises the
	// question: how should we convert from Unicode to the byte sequence?
	//
	// If the string that we are writing is a genuine character string,
	// then probably the best thing to do is the same thing that we do
	// for mercury_print_string(): do the conversion using
	// the System.Encoding.Default encoding, which is the encoding
	// corresponding to the system's code page (character set), which
	// will normally be an 8-bit national character set.  If the Unicode
	// string contains characters which can't be represented in this set,
	// then the encoder will throw an exception.
	//
	// On the other hand, if the string contains binary values which
	// are supposed to be used only for their binary value -- which may
	// be the case if it was constructed using characters which have
	// been obtained using `enum__from_int' (i.e. the reverse mode of
	// `char__to_int'), then probably it would be better to just
	// take the lower 8 bits of the Unicode values, and throw an
	// exception if any of the other bits are set.
	//
	// The documentation for io__write_bytes doesn't make it clear
	// which of these is the case.  It says ``the bytes are taken
	// from a string'', but it doesn't say how.  I will assume
	// that it means the bottom 8 bits of the Unicode value,
	// just like io__write_byte takes the byte from the bottom 8 bits
	// of the int value.

/* XXX possible alternative implementation.
	byte[] byte_array = System.Text.Encoding.Default().GetBytes(s);
*/
	int len = s.Length;
	byte[] byte_array = new byte[len];
	for (int i = 0; i < len; i++) {
		byte_array[i] = (byte) s[i];
		if (byte_array[i] != s[i]) {
			mercury.runtime.Errors.SORRY(
			""write_bytes: Unicode char does not fit in a byte"");
		}
	}
	mf.stream.Write(byte_array, 0, byte_array.Length);
}

").

:- pragma foreign_code("C#", "

// Read in a character.
// This means reading in one or more bytes,
// converting the bytes from the system's default encoding to Unicode,
// and possibly converting CR-LF to newline.
// Returns -1 on error or EOF.
static int
mercury_getc(MR_MercuryFileStruct mf)
{
	int c;

	if (mf.putback != -1) {
		c = mf.putback;
		mf.putback = -1;
		if (c == '\\n') {
			mf.line_number++;
		}
		return c;
	}

	if (mf.reader == null) {
		mf.reader = new System.IO.StreamReader(mf.stream,
				System.Text.Encoding.Default);
	}

	c = mf.reader.Read();
	switch (mf.file_encoding) {
	case ML_file_encoding_kind.ML_raw_binary:
	case ML_file_encoding_kind.ML_Unix_text_encoding:
		if (c == '\\n') {
			mf.line_number++;
		}
		break;
	case ML_file_encoding_kind.ML_OS_text_encoding:
		// First, check if the character we've read matches
		// System.Environment.NewLine.
		// We assume that System.Environment.NewLine is non-null
		// and that System.Environment.NewLine.Length > 0.
		if (c != System.Environment.NewLine[0]) {
			if (c == '\\n') {
				// the input file was ill-formed,
				// e.g. it contained only raw
				// LFs rather than CR-LF.
				// Perhaps we should throw an exception?
				// If not, we still need to treat
				// this as a newline, and thus
				// increment the line counter.
				mf.line_number++;
			}
		} else /* c == NewLine[0] */ {
			switch (System.Environment.NewLine.Length) {
			case 1:
				mf.line_number++;
				c = '\\n';
				break;
			case 2:
				if (mf.reader.Peek() ==
					System.Environment.NewLine[1])
				{
					mf.reader.Read();
					mf.line_number++;
					c = '\\n';
				} else if (c == '\\n') {
					// the input file was ill-formed,
					// e.g. it contained only raw
					// CRs rather than CR-LF. Perhaps
					// we should throw an exception?
					// If not, we still need to treat
					// this as a newline, and thus
					// increment the line counter.
					mf.line_number++;
				}
				break;
			default:
				mercury.runtime.Errors.SORRY(
	""mercury_getc: Environment.NewLine.Length is neither 1 nor 2"");
				break;
			}
		}
		break;
	}
        return c;
}

static void
mercury_ungetc(MR_MercuryFileStruct mf, int code)
{
	if (mf.putback != -1) {
		mercury.runtime.Errors.SORRY(
			""mercury_ungetc: max one character of putback"");
	}
	mf.putback = code;
	if (code == '\\n') {
		mf.line_number--;
	}
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
mercury_close(MercuryFilePtr mf)
{
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
	if (mf == &mercury_stdin ||
	    mf == &mercury_stdout ||
	    mf == &mercury_stderr ||
	    mf == &mercury_stdin_binary ||
	    mf == &mercury_stdout_binary)
	{
		/*
		** The memory for these streams is allocated statically,
		** so there is nothing to free.
		*/
	} else {
  		/*
		** For the accurate GC or no GC cases,
		** we need to explicitly deallocate the memory here,
		** to avoid a memory leak.
		** Note that the accurate collector won't reclaim
		** io_streams, since the io__stream type is defined
		** as a foreign_type.
		*/
  		MR_GC_free(mf);
	}
#endif /* !MR_CONSERVATIVE_GC */
}

").

:- pragma foreign_code("C#", "

static void
mercury_close(MR_MercuryFileStruct mf)
{
	if (mf.reader != null) {
		mf.reader.Close();
		mf.reader = null;
	}
	if (mf.writer != null) {
		mf.writer.Close();
		mf.writer = null;
	}
	mf.stream.Close();
	mf.stream = null;
}

").


:- pragma foreign_code("C", "

int
ML_fprintf(MercuryFilePtr mf, const char *format, ...)
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
	CharCode = mercury_getc(File);
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C", 
	io__read_byte_val(File::in, ByteVal::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ByteVal = mercury_getc(File);
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C", 
	io__putback_char(File::in, Character::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFilePtr mf = File;
	if (Character == '\\n') {
		MR_line_number(*mf)--;
	}
	/* XXX should work even if ungetc() fails */
	if (MR_UNGETCH(*mf, Character) == EOF) {
		mercury_io_error(mf, ""io__putback_char: ungetc failed"");
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__putback_byte(File::in, Character::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io],
"{
	MercuryFilePtr mf = File;
	/* XXX should work even if ungetc() fails */
	if (MR_UNGETCH(*mf, Character) == EOF) {
		mercury_io_error(mf, ""io__putback_byte: ungetc failed"");
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#", 
	io__read_char_code(File::in, CharCode::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure],
"
	MR_MercuryFileStruct mf = File;
	CharCode = mercury_getc(mf);
").

:- pragma foreign_proc("C#", 
	io__read_byte_val(File::in, ByteVal::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure],
"
	MR_MercuryFileStruct mf = File;
	if (mf.putback != -1) {
		ByteVal = mf.putback;
		mf.putback = -1;
	} else {
		ByteVal = mf.stream.ReadByte();
	}
").

:- pragma foreign_proc("C#", 
	io__putback_char(File::in, Character::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure],
"{
	MR_MercuryFileStruct mf = File;
	mercury_ungetc(mf, Character);
}").

:- pragma foreign_proc("C#",
	io__putback_byte(File::in, Byte::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure],
"{
	MR_MercuryFileStruct mf = File;
	if (mf.putback != -1) {
		mercury.runtime.Errors.SORRY(
			""io__putback_byte: max one character of putback"");
	}
	mf.putback = Byte;
}").

/* output predicates - with output to mercury_current_text_output */

:- pragma foreign_proc("C", 
	io__write_string(Message::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	mercury_print_string(mercury_current_text_output, Message);
	MR_update_io(IO0, IO);
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
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_int(Val::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (ML_fprintf(mercury_current_text_output, ""%ld"", (long) Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_float(Val::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
	MR_sprintf_float(buf, Val);
	if (ML_fprintf(mercury_current_text_output, ""%s"", buf) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	MR_update_io(IO0, IO);
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
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__write_bytes(Message::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	mercury_print_binary_string(mercury_current_binary_output, Message);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C", 
	io__flush_output(IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (MR_FLUSH(*mercury_current_text_output) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__flush_binary_output(IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	if (MR_FLUSH(*mercury_current_binary_output) < 0) {
		mercury_output_error(mercury_current_binary_output);
	}
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#", 
	io__write_string(Message::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_print_string(mercury_current_text_output, Message);
").

:- pragma foreign_proc("C#", 
	io__write_char(Character::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	/* See mercury_output_string() for comments */
	if (mercury_current_text_output.writer == null) {
		mercury_current_text_output.writer =
			new System.IO.StreamWriter(
				mercury_current_text_output.stream,
				System.Text.Encoding.Default);
	}
	System.IO.TextWriter w = mercury_current_text_output.writer;
	if (Character == '\\n') {
		switch (mercury_current_text_output.file_encoding) {
		case ML_file_encoding_kind.ML_raw_binary:
		case ML_file_encoding_kind.ML_Unix_text_encoding:
			w.Write(Character);
			break;
		case ML_file_encoding_kind.ML_OS_text_encoding:
			w.WriteLine("""");
			break;
		}
		mercury_current_text_output.line_number++;
	} else {
		w.Write(Character);
	}
").

:- pragma foreign_proc("C#",
	io__write_int(Val::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_print_string(mercury_current_text_output, Val.ToString());
").

:- pragma foreign_proc("C#",
	io__write_byte(Byte::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_current_binary_output.stream.WriteByte(
			System.Convert.ToByte(Byte));
").

:- pragma foreign_proc("C#",
	io__write_bytes(Message::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	mercury_print_binary_string(mercury_current_binary_output, Message);
}").

:- pragma foreign_proc("C#", 
	io__flush_output(_IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_current_text_output.stream.Flush();
").

:- pragma foreign_proc("C#",
	io__flush_binary_output(_IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	mercury_current_binary_output.stream.Flush();
").

io__write_float(Float) -->
	io__write_string(string__float_to_string(Float)).

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
	io__seek_binary_2(Stream::in, Flag::in, Off::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };

	/* XXX should check for failure */
	/* XXX should also check if the stream is seekable */
	if (MR_IS_FILE_STREAM(*Stream)) {
		fseek(MR_file(*Stream), Off, seek_flags[Flag]);
	} else {
		mercury_io_error(Stream,
			""io__seek_binary_2: unseekable stream"");
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__binary_stream_offset(Stream::in, Offset::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	/* XXX should check for failure */
	/* XXX should check if the stream is tellable */
	if (MR_IS_FILE_STREAM(*Stream)) {
		Offset = ftell(MR_file(*Stream));
	} else {
		mercury_io_error(Stream,
			""io__binary_stream_offset: untellable stream"");
	}
	MR_update_io(IO0, IO);
}").

/* output predicates - with output to the specified stream */

:- pragma foreign_proc("C",
	io__write_string(Stream::in, Message::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe], 
"{
	mercury_print_string(Stream, Message);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_char(Stream::in, Character::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe], 
"{
	if (MR_PUTCH(*Stream, Character) < 0) {
		mercury_output_error(Stream);
	}
	if (Character == '\\n') {
		MR_line_number(*Stream)++;
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_int(Stream::in, Val::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	if (ML_fprintf(Stream, ""%ld"", (long) Val) < 0) {
		mercury_output_error(Stream);
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_float(Stream::in, Val::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
	MR_sprintf_float(buf, Val);
	if (ML_fprintf(Stream, ""%s"", buf) < 0) {
		mercury_output_error(Stream);
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_byte(Stream::in, Byte::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	/* call putc with a strictly non-negative byte-sized integer */
	if (MR_PUTCH(*Stream, (int) ((unsigned char) Byte)) < 0) {
		mercury_output_error(Stream);
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__write_bytes(Stream::in, Message::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	mercury_print_binary_string(Stream, Message);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__flush_output(Stream::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	if (MR_FLUSH(*Stream) < 0) {
		mercury_output_error(Stream);
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__flush_binary_output(Stream::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	if (MR_FLUSH(*Stream) < 0) {
		mercury_output_error(Stream);
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__write_string(Stream::in, Message::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io], 
"{
	mercury_print_string(Stream, Message);
}").

:- pragma foreign_proc("C#",
	io__write_char(Stream::in, Character::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io], 
"{
	MR_MercuryFileStruct stream = Stream;
	/* See mercury_output_string() for comments */
	if (stream.writer == null) {
		stream.writer = new System.IO.StreamWriter(stream.stream,
			System.Text.Encoding.Default);
	}
	System.IO.TextWriter w = stream.writer;
	if (Character == '\\n') {
		switch (stream.file_encoding) {
		case ML_file_encoding_kind.ML_raw_binary:
		case ML_file_encoding_kind.ML_Unix_text_encoding:
			w.Write(Character);
			break;
		case ML_file_encoding_kind.ML_OS_text_encoding:
			w.WriteLine("""");
			break;
		}
		stream.line_number++;
	} else {
		w.Write(Character);
	}
}").

:- pragma foreign_proc("C#",
	io__write_int(Stream::in, Val::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	mercury_print_string(Stream, Val.ToString());
}").

:- pragma foreign_proc("C#",
	io__write_byte(Stream::in, Byte::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	Stream.stream.WriteByte(System.Convert.ToByte(Byte));
}").

:- pragma foreign_proc("C#",
	io__write_bytes(Stream::in, Message::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	mercury_print_binary_string(Stream, Message);
}").

:- pragma foreign_proc("C#",
	io__flush_output(Stream::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	Stream.stream.Flush();
}").

:- pragma foreign_proc("C#",
	io__flush_binary_output(Stream::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
	Stream.stream.Flush();
}").

io__write_float(Stream, Float) -->
	io__write_string(Stream, string__float_to_string(Float)).

/* stream predicates */

:- pragma export(io__stdin_stream(out, di, uo), "ML_io_stdin_stream").
:- pragma export(io__stdout_stream(out, di, uo), "ML_io_stdout_stream").
:- pragma export(io__stderr_stream(out, di, uo), "ML_io_stderr_stream").

:- pragma foreign_proc("C",
	io__stdin_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = &mercury_stdin;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stdout_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = &mercury_stdout;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stderr_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = &mercury_stderr;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stdin_binary_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = &mercury_stdin_binary;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__stdout_binary_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = &mercury_stdout_binary;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__input_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_text_input;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__output_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_text_output;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__binary_input_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_binary_input;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__binary_output_stream(Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_binary_output;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__get_line_number(LineNum::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = MR_line_number(*mercury_current_text_input);
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__get_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	LineNum = MR_line_number(*Stream);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__set_line_number(LineNum::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_line_number(*mercury_current_text_input) = LineNum;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MR_line_number(*Stream) = LineNum;
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__get_output_line_number(LineNum::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = MR_line_number(*mercury_current_text_output);
	MR_update_io(IO0, IO);
").
	
:- pragma foreign_proc("C",
	io__get_output_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	LineNum = MR_line_number(*Stream);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__set_output_line_number(LineNum::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	MR_line_number(*mercury_current_text_output) = LineNum;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_output_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	MR_line_number(*Stream) = LineNum;
	MR_update_io(IO0, IO);
}").

current_input_stream(S) --> input_stream(S).
current_output_stream(S) --> output_stream(S).
current_binary_input_stream(S) --> binary_input_stream(S).
current_binary_output_stream(S) --> binary_output_stream(S).

% io__set_input_stream(NewStream, OldStream, IO0, IO1)
%	Changes the current input stream to the stream specified.
%	Returns the previous stream.
:- pragma foreign_proc("C",
	io__set_input_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_text_input;
	mercury_current_text_input = NewStream;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_output_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_text_output;
	mercury_current_text_output = NewStream;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_binary_input_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_binary_input;
	mercury_current_binary_input = NewStream;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_binary_output_stream(NewStream::in, OutStream::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_binary_output;
	mercury_current_binary_output = NewStream;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
	io__stdin_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	Stream = mercury_stdin;
").

:- pragma foreign_proc("C#",
	io__stdout_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	Stream = mercury_stdout;
").

:- pragma foreign_proc("C#",
	io__stderr_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	Stream = mercury_stderr;
").

:- pragma foreign_proc("C#",
	io__stdin_binary_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	Stream = mercury_stdin_binary;
").

:- pragma foreign_proc("C#",
	io__stdout_binary_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	Stream = mercury_stdout_binary;
").

:- pragma foreign_proc("C#",
	io__input_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_text_input;
").

:- pragma foreign_proc("C#",
	io__output_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_text_output;
").

:- pragma foreign_proc("C#",
	io__binary_input_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_binary_input;
").

:- pragma foreign_proc("C#",
	io__binary_output_stream(Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Stream = mercury_current_binary_output;
").

:- pragma foreign_proc("C#",
	io__get_line_number(LineNum::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = mercury_current_text_input.line_number;
").

:- pragma foreign_proc("C#",
	io__get_line_number(Stream::in, LineNum::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	LineNum = Stream.line_number;
}").

:- pragma foreign_proc("C#",
	io__set_line_number(LineNum::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	mercury_current_text_input.line_number = LineNum;
").

:- pragma foreign_proc("C#",
	io__set_line_number(Stream::in, LineNum::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
	io__get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	LineNum = mercury_current_text_output.line_number;
").

:- pragma foreign_proc("C#",
	io__get_output_line_number(Stream::in, LineNum::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	LineNum = Stream.line_number;
}").

:- pragma foreign_proc("C#",
	io__set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	mercury_current_text_output.line_number = LineNum;
").

:- pragma foreign_proc("C#",
	io__set_output_line_number(Stream::in, LineNum::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	Stream.line_number = LineNum;
}").

% io__set_input_stream(NewStream, OldStream, IO0, IO1)
%	Changes the current input stream to the stream specified.
%	Returns the previous stream.
:- pragma foreign_proc("C#",
	io__set_input_stream(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_text_input;
	mercury_current_text_input = NewStream;
").

:- pragma foreign_proc("C#",
	io__set_output_stream(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_text_output;
	mercury_current_text_output = NewStream;
").

:- pragma foreign_proc("C#",
	io__set_binary_input_stream(NewStream::in, OutStream::out,
		_IO0::di, _IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_binary_input;
	mercury_current_binary_input = NewStream;
").

:- pragma foreign_proc("C#",
	io__set_binary_output_stream(NewStream::in, OutStream::out,
		_IO0::di, _IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	OutStream = mercury_current_binary_output;
	mercury_current_binary_output = NewStream;
").

/* stream open/close predicates */

%	io__do_open_binary(File, Mode, ResultCode, StreamId, Stream, IO0, IO):
%	io__do_open_text(File, Mode, ResultCode, StreamId, Stream, IO0, IO):
%		Attempts to open a file in the specified mode.
%		The Mode is a string suitable for passing to fopen().
%		Result is 0 for success, -1 for failure.
%		StreamId is a unique integer identifying the open.
%		Both StreamId and Stream are valid only if Result == 0.

:- pred io__do_open_binary(string::in, string::in, int::out, int::out,
	io__input_stream::out, io__state::di, io__state::uo) is det.

:- pred io__do_open_text(string::in, string::in, int::out, int::out,
	io__input_stream::out, io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__do_open_text(FileName::in, Mode::in, ResultCode::out,
		StreamId::out, Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = mercury_open(FileName, Mode);
	ResultCode = (Stream != NULL ? 0 : -1);
	StreamId = (Stream != NULL ? ML_next_stream_id++ : -1);
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__do_open_binary(FileName::in, Mode::in, ResultCode::out,
		StreamId::out, Stream::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	Stream = mercury_open(FileName, Mode);
	ResultCode = (Stream != NULL ? 0 : -1);
	StreamId = (Stream != NULL ? ML_next_stream_id++ : -1);
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
	io__do_open_text(FileName::in, Mode::in, ResultCode::out,
		StreamId::out, Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	MR_MercuryFileStruct mf = mercury_open(FileName, Mode,
		ML_default_text_encoding);
	Stream = mf;
	if (mf != null) {
		ResultCode = 0;
		StreamId = mf.id;
	} else {
		ResultCode = -1;
		StreamId = -1;
	}
").

:- pragma foreign_proc("C#",
	io__do_open_binary(FileName::in, Mode::in, ResultCode::out,
		StreamId::out, Stream::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	MR_MercuryFileStruct mf = mercury_open(FileName, Mode,
			ML_file_encoding_kind.ML_raw_binary);
	Stream = mf;
	if (mf != null) {
		ResultCode = 0;
		StreamId = mf.id;
	} else {
		ResultCode = -1;
		StreamId = -1;
	}
").

io__close_input(Stream) -->
	io__maybe_delete_stream_info(Stream),
	io__close_stream(Stream).

io__close_output(Stream) -->
	io__maybe_delete_stream_info(Stream),
	io__close_stream(Stream).

io__close_binary_input(Stream) -->
	io__maybe_delete_stream_info(Stream),
	io__close_stream(Stream).

io__close_binary_output(Stream) -->
	io__maybe_delete_stream_info(Stream),
	io__close_stream(Stream).

:- pred io__close_stream(stream::in, io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__close_stream(Stream::in, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	mercury_close(Stream);
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
	io__close_stream(Stream::in, _IO0::di, _IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	mercury_close(Stream);
").

/* miscellaneous predicates */

:- pragma foreign_proc("C",
	io__progname(DefaultProgname::in, PrognameOut::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
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
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__command_line_arguments(Args::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	int	i;

	/* convert mercury_argv from a vector to a list */
	i = mercury_argc;
	Args = MR_list_empty_msg(MR_PROC_LABEL);
	while (--i >= 0) {
		Args = MR_string_list_cons_msg((MR_Word) mercury_argv[i], Args,
			MR_PROC_LABEL);
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
	io__get_exit_status(ExitStatus::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io], 
"
	ExitStatus = mercury_exit_status;
	MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
	io__set_exit_status(ExitStatus::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	mercury_exit_status = ExitStatus;
	MR_update_io(IO0, IO);
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
		ML_maybe_make_err_msg(MR_TRUE, errno,
			""error invoking system command: "",
			MR_PROC_LABEL, Msg);
	} else {
		Msg = MR_make_string_const("""");	
	}
	MR_update_io(IO0, IO);
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

:- pragma foreign_proc("C#",
	io__command_line_arguments(Args::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
	string[] arg_vector = System.Environment.GetCommandLineArgs();
	int i = arg_vector.Length;
	Args = mercury.list.mercury_code.ML_empty_list(null);
		// We don't get the 0th argument: it is the executable name
	while (--i > 0) {
		Args = mercury.list.mercury_code.ML_cons(null,
				arg_vector[i], Args);
	}
").

:- pragma foreign_proc("C#",
	io__get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	ExitStatus = System.Environment.ExitCode;
").

:- pragma foreign_proc("C#",
	io__set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	System.Environment.ExitCode = ExitStatus;
").

:- pragma foreign_proc("C#",
	io__call_system_code(Command::in, Status::out, Msg::out,
			_IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	try {
		// XXX This could be better... need to handle embedded spaces
		//     in the command name.
		int index = Command.IndexOf("" "");
		string command = Command.Substring(0, index);
		string arguments = Command.Remove(0, index + 1);

		// debugging...
		// System.Console.Out.WriteLine(
		// 	""[command = "" + command + ""]"");
		// System.Console.Out.WriteLine(
		// 	""[arguments = "" + arguments + ""]"");

		System.Diagnostics.Process process =
			System.Diagnostics.Process.Start(command, arguments);
		process.WaitForExit();
		Status = process.ExitCode;
		Msg = """";

		// debugging...
		// System.Console.Out.WriteLine(
		// 	""[exitcode = "" + Status + ""]"");

	}
	catch (System.Exception e) {
		Status = 127;
		Msg = e.Message;

		// debugging...
		// System.Console.Out.WriteLine(
		// 	""[message = "" + Msg + ""]"");
	}
").

/*---------------------------------------------------------------------------*/

/* io__getenv and io__setenv */

:- pragma foreign_decl("C", "
#include <stdlib.h>	/* for getenv() and putenv() */
").

:- pragma promise_semipure(io__getenv/2).

:- pragma foreign_proc("C",
	io__getenv(Var::in, Value::out),
	[will_not_call_mercury, tabled_for_io],
"{
	Value = getenv(Var);
	SUCCESS_INDICATOR = (Value != 0);
}").

:- pragma foreign_proc("C#",
	io__getenv(Var::in, Value::out),
	[will_not_call_mercury, tabled_for_io],
"{
	Value = System.Environment.GetEnvironmentVariable(Var);
	SUCCESS_INDICATOR = (Value != null);
}").

io__setenv(Var, Value) :-
	impure io__putenv(Var ++ "=" ++ Value).


:- impure pred io__putenv(string).
:- mode io__putenv(in) is semidet.
%	io__putenv(VarString).
%		If VarString is a string of the form "name=value",
%		sets the environment variable name to the specified
%		value.  Fails if the operation does not work.
%		Not supported for .NET.
%		This should only be called from io__setenv.

:- pragma foreign_proc("C",
	io__putenv(VarAndValue::in),
	[will_not_call_mercury, tabled_for_io],
"
	SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);
").

:- pragma foreign_proc("C#",
	io__putenv(VarAndValue::in),
	[will_not_call_mercury, tabled_for_io],
"
	/*
	** Unfortunately there is no API in the .NET standard library
	** for setting environment variables.  So we need to use
	** platform-specific methods.  Currently we use the Posix function
	** putenv(), which is also supported on Windows.
	*/
	SUCCESS_INDICATOR = (mercury.runtime.PInvoke._putenv(VarAndValue) == 0);
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
	{ Var = ( dir__use_windows_paths -> "TMP" ; "TMPDIR" ) },
	io__get_environment_var(Var, Result),
	(
		{ Result = yes(Dir) }
	;
		{ Result = no },
		{ dir__use_windows_paths ->
			Dir = dir__this_directory
		;
			Dir = "/tmp"
		}
	),
	io__make_temp(Dir, "mtmp", Name).

io__make_temp(Dir, Prefix, Name) -->
	io__do_make_temp(Dir, Prefix, char_to_string(dir__directory_separator),
		Name, Err, Message),
	{ Err \= 0 ->
		throw_io_error(Message)
	;
		true
	}.

/*---------------------------------------------------------------------------*/

:- pred io__do_make_temp(string, string, string, string, int, string,
	io__state, io__state).
:- mode io__do_make_temp(in, in, in, out, out, out, di, uo) is det.

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

	#define	ML_MAX_TEMPNAME_TRIES	(6 * 4)

	extern long ML_io_tempnam_counter;
").

:- pragma foreign_code("C", "
	long	ML_io_tempnam_counter = 0;
").

:- pragma foreign_proc("C",
	io__do_make_temp(Dir::in, Prefix::in, Sep::in, FileName::out,
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
	MR_offset_incr_hp_atomic_msg(MR_LVALUE_CAST(MR_Word, FileName), 0,
		(len + sizeof(MR_Word)) / sizeof(MR_Word),
		MR_PROC_LABEL, ""string:string/0"");
	if (ML_io_tempnam_counter == 0) {
		ML_io_tempnam_counter = getpid();
	}
	num_tries = 0;
	do {
		sprintf(countstr, ""%06lX"", ML_io_tempnam_counter & 0xffffffL);
		strcpy(FileName, Dir);
		strcat(FileName, Sep);
		strncat(FileName, Prefix, 5);
		strncat(FileName, countstr, 3);
		strcat(FileName, ""."");
		strncat(FileName, countstr + 3, 3);
		fd = open(FileName, O_WRONLY | O_CREAT | O_EXCL, 0600);
		num_tries++;
		ML_io_tempnam_counter += (1 << num_tries);
	} while (fd == -1 && errno == EEXIST &&
		num_tries < ML_MAX_TEMPNAME_TRIES);
	if (fd == -1) {
		ML_maybe_make_err_msg(MR_TRUE, errno,
			""error opening temporary file: "",
			MR_PROC_LABEL, ErrorMessage);
		Error = -1;
	}  else {
		err = close(fd);
		ML_maybe_make_err_msg(err, errno,
			""error closing temporary file: "",
			MR_PROC_LABEL, ErrorMessage);
		Error = err;
	}
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__do_make_temp(Dir::in, Prefix::in, _Sep::in, FileName::out,
		Error::out, ErrorMessage::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	/* XXX For some reason this MC++ code below doesn't work.
	 * We get the following error message:
	 *   Unhandled Exception: System.TypeInitializationException: The type
	 *   initializer for ""remove_file.mercury_code"" threw an exception.
	 *   ---> System.IO.FileLoadException: The dll initialization routine
	 *   failed for file 'io__cpp_code.dll'.
	 * so instead we use the .NET call and ignore Dir and Prefix.
	int result;
	char __nogc *dir = static_cast<char*>(
			System::Runtime::InteropServices::Marshal::
			StringToHGlobalAnsi(Dir).ToPointer());;
	char __nogc *prefix = static_cast<char*>(
			System::Runtime::InteropServices::Marshal::
			StringToHGlobalAnsi(Prefix).ToPointer());;
	char tmpFileName[MAX_PATH];
	System::String *msg[] = {
			S""Unable to create temporary file in "",
			Dir,
			S"" with prefix "",
			Prefix
		};


	result = GetTempFileName(dir, prefix, 0, tmpFileName);

	if (result == 0) {
		Error = -1;
		FileName = S"""";
		ErrorMessage = System::String::Join(S"""", msg);
	} else {
		Error = 0;
		FileName = tmpFileName;
		ErrorMessage = S"""";
	}
	*/

	try {
		FileName = System.IO.Path.GetTempFileName();
		Error = 0;
		ErrorMessage = """";
	}
	catch (System.Exception e)
	{
		FileName = """";
		Error = -1;
		ErrorMessage = e.Message;
	}
}").

/*---------------------------------------------------------------------------*/

:- pragma foreign_decl("C", "

#include <string.h>
#include <errno.h>

/*
** ML_maybe_make_err_msg(was_error, errno, msg, procname, error_msg):
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
** It also needs to be a macro because MR_offset_incr_hp_atomic_msg()
** stringizes the procname argument.
*/
#define ML_maybe_make_err_msg(was_error, error, msg, procname, error_msg) \\
	do {								\\
		char *errno_msg;					\\
		size_t total_len;					\\
		MR_Word tmp;						\\
									\\
		if (was_error) {					\\
			errno_msg = strerror(error);			\\
			total_len = strlen(msg) + strlen(errno_msg);	\\
			MR_offset_incr_hp_atomic_msg(tmp, 0,		\\
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

/*
** ML_maybe_make_win32_err_msg(was_error, error, msg, procname, error_msg):
**	if `was_error' is true, then append `msg' and the string
**	returned by the Win32 API function FormatMessage() for the
**	last error to give `error_msg'; otherwise, set `error_msg' to NULL.
**	Aborts if MR_WIN32 is not defined.
**
** WARNING: this must only be called when the `hp' register is valid.
** That means it must only be called from procedures declared
** `[will_not_call_mercury]'.
**
** This is defined as a macro rather than a C function
** to avoid worrying about the `hp' register being
** invalidated by the function call.
** It also needs to be a macro because MR_incr_hp_atomic_msg()
** stringizes the procname argument.
*/
#ifdef MR_WIN32

#include <windows.h>

#define ML_maybe_make_win32_err_msg(was_error, error, msg, procname, error_msg) \\
	do {								\\
		size_t total_len;					\\
		MR_Word tmp;						\\
									\\
		if (was_error) {					\\
			LPVOID err_buf;					\\
			MR_bool free_err_buf = MR_TRUE;			\\
			if (!FormatMessage(				\\
				FORMAT_MESSAGE_ALLOCATE_BUFFER		\\
				| FORMAT_MESSAGE_FROM_SYSTEM		\\
				| FORMAT_MESSAGE_IGNORE_INSERTS,	\\
				NULL,					\\
				error,					\\
				MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), \\
				(LPTSTR) &err_buf,			\\
				0,					\\
				NULL))					\\
			{						\\
				free_err_buf = MR_FALSE;		\\
				err_buf = ""could not retrieve error message""; \\
			}						\\
			total_len = strlen(msg) + strlen((char *)err_buf); \\
			MR_incr_hp_atomic_msg(tmp,			\\
				(total_len + sizeof(MR_Word))		\\
					/ sizeof(MR_Word),		\\
				procname,				\\
				""string:string/0"");			\\
			(error_msg) = (char *)tmp;			\\
			strcpy((error_msg), msg);			\\
			strcat((error_msg), (char *)err_buf);		\\
			if (free_err_buf) {				\\
				LocalFree(err_buf);			\\
			}						\\
		} else {						\\
			(error_msg) = NULL;				\\
		}							\\
	} while(0)

#else /* !MR_WIN32 */

#define ML_maybe_make_win32_err_msg(was_error, error, msg, procname, error_msg) \\
	MR_fatal_error(""ML_maybe_make_win32_err_msg called on non-Windows platform"")

#endif /* !MR_WIN32 */

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
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	RetVal = remove(FileName);
	ML_maybe_make_err_msg(RetVal != 0, errno, ""remove failed: "",
		MR_PROC_LABEL, RetStr);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__remove_file_2(FileName::in, RetVal::out, RetStr::out,
		_IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	try {
		if (System.IO.File.Exists(FileName)) {
			System.IO.File.Delete(FileName);
			RetVal = 0;
			RetStr = """";
		} else {
			RetVal = -1;
			RetStr = ""remove failed: No such file or directory"";
		}
	}
	catch (System.Exception e) {
		RetVal = -1;
		RetStr = e.Message;
	}
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

:- pragma foreign_proc("C",
	io__rename_file_2(OldFileName::in, NewFileName::in, RetVal::out,
		RetStr::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	RetVal = rename(OldFileName, NewFileName);
	ML_maybe_make_err_msg(RetVal != 0, errno, ""rename failed: "",
		MR_PROC_LABEL, RetStr);
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
	io__rename_file_2(OldFileName::in, NewFileName::in, RetVal::out,
		RetStr::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
	try {
		if (System.IO.File.Exists(OldFileName)) {
			System.IO.File.Move(OldFileName, NewFileName);
			RetVal = 0;
			RetStr = """";
		} else {
			RetVal = -1;
			RetStr = ""rename failed: No such file or directory"";
		}
	}
	catch (System.Exception e) {
		RetVal = -1;
		RetStr = e.Message;
	}
}").

io__have_symlinks :- semidet_fail.

:- pragma foreign_proc("C", io__have_symlinks,
		[will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_HAVE_SYMLINK) && defined(MR_HAVE_READLINK)
	SUCCESS_INDICATOR = MR_TRUE;
#else
	SUCCESS_INDICATOR = MR_FALSE;
#endif
").

io__make_symlink(FileName, LinkFileName, Result) -->
	( { io__have_symlinks } ->
		io__make_symlink_2(FileName, LinkFileName, Status),
		( { Status = 0 } ->
			io__make_err_msg("io.make_symlink failed: ", Msg),
			{ Result = error(make_io_error(Msg)) }
		;
			{ Result = ok }
		)
	;
		{ Result = error(make_io_error(
			"io.make_symlink not supported on this platform")) }
	).

:- pred io__make_symlink_2(string::in, string::in, int::out,
		io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__make_symlink_2(FileName::in, LinkFileName::in,
		Status::out, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_SYMLINK
	Status = (symlink(FileName, LinkFileName) == 0);
#else
	Status = 0;
#endif
	MR_update_io(IO0, IO);
}").

io__read_symlink(FileName, Result) -->
	( { io__have_symlinks } ->
		io__read_symlink_2(FileName, TargetFileName, Status, Error),
		( { Status = 0 } ->
			io__make_err_msg(Error,
				"io.read_symlink failed: ", Msg),
			{ Result = error(make_io_error(Msg)) }
		;
			{ Result = ok(TargetFileName) }
		)
	;
		{ Result = error(make_io_error(
			"io.read_symlink not supported on this platform")) }
	).

:- pred io__read_symlink_2(string::in, string::out, int::out,
		io__system_error::out, io__state::di, io__state::uo) is det.

:- pragma foreign_proc("C",
	io__read_symlink_2(FileName::in, TargetFileName::out,
		Status::out, Error::out, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_READLINK
  #ifndef PATH_MAX
  #define PATH_MAX 256
  #endif
	int num_chars;
	char *buffer2 = NULL;
	int buffer_size2 = PATH_MAX;
	char buffer[PATH_MAX + 1];

	/* readlink() does not null-terminate the buffer */
	num_chars = readlink(FileName, buffer, PATH_MAX);

	if (num_chars == PATH_MAX) {
		do {
			buffer_size2 *= 2;
			buffer2 = MR_RESIZE_ARRAY(buffer2, char, buffer_size2);
			num_chars = readlink(FileName, buffer2, PATH_MAX);
		} while (num_chars == buffer_size2);
		if (num_chars == -1) {
			Error = errno;
			TargetFileName = MR_make_string_const("""");
			Status = 0;
		} else {
			buffer2[num_chars] = '\\0';
			MR_make_aligned_string_copy(TargetFileName, buffer2);
			Status = 1;
		}
		MR_free(buffer2);
	} else if (num_chars == -1) {
		TargetFileName = MR_make_string_const("""");
		Error = errno;
		Status = 0;
	} else {
		buffer[num_chars] = '\\0';
		MR_make_aligned_string_copy(TargetFileName,
			buffer);
		Status = 1;	
	}
#else /* !MR_HAVE_READLINK */
	TargetFileName = NULL;
	Status = 0;
#endif
	MR_update_io(IO0, IO);
}").


/*---------------------------------------------------------------------------*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%	Functional forms added.

io__error_message(Error) = Msg :-
	io__error_message(Error, Msg).
