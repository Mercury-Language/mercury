%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: io.m.
% Main author: fjh.
% Stability: medium to high (but see the caveats about unique modes below).
%
% This file encapsulates all the file I/O.
% We implement a purely logical I/O system using non-logical I/O primitives
% of the underlying system (C or Prolog).
% The logicalness is enforced at compile time by using unique modes.
% (Except that we haven't implemented that yet.  So instead it's checked at
% runtime.  Except that those checks prevent doing a `redo' in the debugger.
% So we don't check at all.  Oh well.  Just don't write any programs that
% depend on it, because they *WILL* break!)
% Late news: unique mode checking is now implemented - well, mostly ;-).
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.
:- import_module char, int, float, string, std_util, list.

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

	% Opaque handles for I/O streams.

:- type io__input_stream.

:- type io__output_stream.

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

:- type io__error.	% Use io__error_message to decode it.

	% Poly-type is used for io__write_many, which does
	% some vaguely printf-like formatting.

:- type io__poly_type == string__poly_type.
%			--->
%		c(char)
%	;	s(string)
%	;	i(int)
%	;	f(float).
%

%-----------------------------------------------------------------------------%

% Input predicates.

:- pred io__read_char(io__result(char), io__state, io__state).
:- mode io__read_char(out, di, uo) is det.
%		Reads a character from the current input stream.

:- pred io__read_word(io__result(list(char)), io__state, io__state).
:- mode io__read_word(out, di, uo) is det.
%		Reads a whitespace delimited word from the current input stream.

:- pred io__read_line(io__result(list(char)), io__state, io__state).
:- mode io__read_line(out, di, uo) is det.
%		Reads a line from the current input stream.

:- pred io__putback_char(char, io__state, io__state).
:- mode io__putback_char(in, di, uo) is det.
%		Un-reads a character from the current input stream.
%		You can put back as many characters as you like.
%		You can even put back something that you didn't actually read.

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
%		Reads a line from specified stream.

:- pred io__putback_char(io__input_stream, char, io__state, io__state).
:- mode io__putback_char(in, in, di, uo) is det.
%		Un-reads a character from specified stream.
%		You can put back as many characters as you like.
%		You can even put back something that you didn't actually read.

:- pred io__read_anything(io__read_result(T), io__state, io__state).
:- mode io__read_anything(out, di, uo) is det.
%		Reads its argument from the current input stream.
%		The argument may be of (almost) any type. 
%		The term read had better be of the right type!
%		XXX io__read_anything is NOT YET IMPLEMENTED.
%		It will also probably be renamed io__read.

:- pred io__read_anything(io__input_stream, io__read_result(T),
							io__state, io__state).
:- mode io__read_anything(in, out, di, uo) is det.
%		Reads its argument to the specified stream.
%		The argument may be of (almost) any type.
%		The term read had better be of the right type!
%		XXX io__read_anything is NOT YET IMPLEMENTED.
%		It will also probably be renamed io__read.

:- pred io__ignore_whitespace(io__result, io__state, io__state).
:- mode io__ignore_whitespace(out, di, uo) is det.
%		Discards all the whitespace from the current stream.

:- pred io__ignore_whitespace(io__input_stream, io__result,
				io__state, io__state).
:- mode io__ignore_whitespace(in, out, di, uo) is det.
%		Discards all the whitespace from the specified stream.



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

:- pred io__write_char(char, io__state, io__state).
:- mode io__write_char(in, di, uo) is det.
%		Writes a character to the current output stream.

:- pred io__write_char(io__output_stream, char, io__state, io__state).
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

:- pred io__write_many(list(io__poly_type), io__state, io__state).
:- mode io__write_many(in, di, uo) is det.
%	writes a polyglot to output.

:- pred io__write_many(io__output_stream, list(io__poly_type), io__state, io__state).
:- mode io__write_many(in, in, di, uo) is det.
%	writes a polyglot to a specified stream.

:- pred io__write_anything(T, io__state, io__state).
:- mode io__write_anything(in, di, uo) is det.
%		Writes its argument to the current output stream.
%		The argument may be of (almost) any type.
%		(Any type except a higher-order predicate type,
%		or some of the builtin types such as io__state itself.)
%		XXX io__write_anything is NOT YET IMPLEMENTED.
%		It will probably also be renamed `io__write'.

:- pred io__write_anything(io__output_stream, T, io__state, io__state).
:- mode io__write_anything(in, in, di, uo) is det.
%		Writes its argument to the specified stream.
%		The argument may be of (almost) any type.
%		(Any type except a higher-order predicate type,
%		or some of the builtin types such as io__state itself.)
%		XXX io__write_anything is NOT YET IMPLEMENTED.
%		It will probably also be renamed `io__write'.

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
%	io__open_input(File, Result, IO0, IO1).
%		Attempts to open a file for input.
%		Result is either 'ok(Stream)' or 'error(ErrorCode)'.

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

:- pred io__get_line_number(io__input_stream, int, io__state, io__state).
:- mode io__get_line_number(in, out, di, uo) is det.

%	Return the line number of the current input stream.
%	Lines are numbered starting at 1.

%-----------------------------------------------------------------------------%

% Output stream predicates.

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
%	For stdout this is the string "<standard input>".
%	For stderr this is the string "<standard error>".

:- pred io__output_stream_name(io__output_stream, string, io__state, io__state).
:- mode io__output_stream_name(in, out, di, uo) is det.
%	Retrieves the human-readable name associated with the specified stream.
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

% The io__state contains an integer used to record the program's exit status.
% When the program finishes, it will return this exit status to the operating
% system.  The following predicates can be used to get and set the exit status.

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
	% variable.  Will abort if the system runs out of environment
	% space.

%-----------------------------------------------------------------------------%

% Memory management predicates.

	% Write some memory/time usage statistics to stdout.

:- pred io__report_stats(io__state, io__state).
:- mode io__report_stats(di, uo) is det.

	% Preallocate heap space (to avoid NU-Prolog panic).

:- pred io__preallocate_heap_space(int, io__state, io__state).
:- mode io__preallocate_heap_space(in, di, uo) is det.

:- pred io__gc_call(pred(io__state, io__state), io__state, io__state).
:- mode io__gc_call(pred(di, uo) is det, di, uo) is det.
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
%		possible to invoke the command, or `error(ErrorCode)' if not.

:- pred io__error_message(io__error, string).
:- mode io__error_message(in, out) is det.
%	io__error_message(ErrorCode, ErrorMessage).
%		Look up the error message corresponding to a particular error
%		code.

%-----------------------------------------------------------------------------%

% For use by term_io.m:

:- import_module ops.

:- pred io__get_op_table(ops__table, io__state, io__state).
:- mode io__get_op_table(out, di, uo) is det.

:- pred io__set_op_table(ops__table, io__state, io__state).
:- mode io__set_op_table(di, di, uo) is det.

% For use by the Mercury runtime (io.mod):

:- type io__external_state.

:- pred io__init_state(io__external_state, io__state).
:- mode io__init_state(di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, dir, term_io, varset, require.

:- type io__state
	---> 	io__state(
			io__stream_names,	% map from stream to stream name
			io__stream_putback,	% map from input stream to
						% list of putback characters
						% Note: only used for the Prolog
						% implementation.
			ops__table, 		% current operators
			univ,			% for use by the application
			io__external_state
		).

:- type io__stream_names ==	map(io__stream, string).
:- type io__stream_putback ==	map(io__stream, list(char)).

:- type io__input_stream ==	io__stream.
:- type io__output_stream ==	io__stream.

:- type io__stream.
/*
 * In NU-Prolog: 
 *	io__stream	--->	stream(int, int)
 *			;	user_input
 *			;	user_output
 *			;	user_error.
 * In C:
 *	io__stream	==	pointer to MercuryStream
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
%	io__call_system(Command, Status, IO0, IO1).
%		Invokes the operating system shell with the specified
%		Command.  Returns Status = -1 on failure.

:- pred io__do_open_input(string, int, io__input_stream, io__state, io__state).
:- mode io__do_open_input(in, out, out, di, uo) is det.
%	io__do_open_input(File, ResultCode, Stream, IO0, IO1).
%		Attempts to open a file for input.
%		Result is 0 for success, -1 for failure.

:- pred io__do_open_output(string, int, io__output_stream, io__state,
							io__state).
:- mode io__do_open_output(in, out, out, di, uo) is det.
%	io__do_open_output(File, ResultCode, Stream, IO0, IO1).
%		Attempts to open a file for output.
%		Result is 0 for success, -1 for failure.

:- pred io__do_open_append(string, int, io__output_stream, io__state,
							io__state).
:- mode io__do_open_append(in, out, out, di, uo) is det.
%	io__do_open_append(File, ResultCode, Stream, IO0, IO1).
%		Attempts to open a file for appending.
%		Result is 0 for success, -1 for failure.

:- pred io__getenv(string, string).
:- mode io__getenv(in, out) is semidet.
%	io__getenv(Var, Value).
%		Gets the value Value associated with the environment
%		variable Var.  Fails if the variable was not set.

:- pred io__putenv(string).
:- mode io__putenv(in) is semidet.
%	io__putenv(VarString).
%		If VarString is a string of the form "name=value",
%		sets the environment variable name to the specified
%		value.  Fails if the operation does not work.

/* Many of these predicates are implemented using either non-logical
   NU-Prolog code in io.nu.nl, or C code in code/io.mod.
*/

:- external(io__progname/4).
:- external(io__read_char_code/4).
:- external(io__putback_char/4).
:- external(io__write_char/3).
:- external(io__write_char/4).
:- external(io__write_int/3).
:- external(io__write_int/4).
:- external(io__write_string/3).
:- external(io__write_string/4).
:- external(io__write_float/3).
:- external(io__write_float/4).
:- external(io__flush_output/2).
:- external(io__flush_output/3).
:- external(io__stdin_stream/3).
:- external(io__stdout_stream/3).
:- external(io__stderr_stream/3).
:- external(io__input_stream/3).
:- external(io__output_stream/3).
:- external(io__set_input_stream/4).
:- external(io__set_output_stream/4).
:- external(io__do_open_input/5).
:- external(io__do_open_output/5).
:- external(io__do_open_append/5).
:- external(io__close_input/3).
:- external(io__close_output/3).
:- external(io__get_line_number/3).
:- external(io__get_line_number/4).
:- external(io__command_line_arguments/3).
:- external(io__get_exit_status/3).
:- external(io__set_exit_status/3).
:- external(io__getenv/2).
:- external(io__putenv/1).
:- external(io__call_system_code/4).
:- external(io__gc_call/3).
:- external(io__preallocate_heap_space/3).

%-----------------------------------------------------------------------------%

% input predicates

io__read_char(Result) -->
	io__input_stream(Stream),
	io__read_char(Stream, Result).

io__read_char(Stream, Result, IO_0, IO) :-
	io__read_char_code(Stream, Code, IO_0, IO),
	(
		Code = -1
	->
		Result = eof
	;
		char__to_int(Char, Code)
	->
		Result = ok(Char)
	;
		% XXX improve error message
		Result = error("read error")
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
	io__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		( { Char = '\n' } ->
			{ Result = ok([Char]) }
		;
			io__read_line(Stream, Result0),
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

io__putback_char(Char) -->
	io__input_stream(Stream),
	io__putback_char(Stream, Char).

io__read_anything(X) -->
	term_io__read_term(ReadResult),
	(	{ ReadResult = term(_VarSet, Term) },
		( { term_to_type(Term, Type) } ->
			{ X = ok(Type) }
		;
			{ X = error("io__read_anything : the term read was not a valid type", 0) }
		)
	;
		{ ReadResult = eof },
		{ X = eof }
	;
		{ ReadResult = error(String, Int) },
		{ X = error(String, Int) }
	).

io__read_anything(Stream, X) -->
	io__set_output_stream(Stream, OrigStream),
	io__read_anything(X),
	io__set_output_stream(OrigStream, _Stream).

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

io__write_strings(Strings) -->
	io__output_stream(Stream),
	io__write_strings(Stream, Strings).

io__write_strings(_Stream, []) --> [].
io__write_strings(Stream, [S|Ss]) -->
	io__write_string(Stream, S),
	io__write_strings(Stream, Ss).

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

io__write_anything(X) -->
	{ type_to_term(X, Term) },
	{ varset__init(VarSet) },
	term_io__write_term(VarSet, Term).

io__write_anything(Stream, X) -->
	{ type_to_term(X, Term) },
	{ varset__init(VarSet) },
	io__set_input_stream(Stream, OrigStream),
	term_io__write_term(VarSet, Term),
	io__set_input_stream(OrigStream, _Stream).

%-----------------------------------------------------------------------------%

% stream predicates

io__open_input(FileName, Result) -->
	io__do_open_input(FileName, Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't open input file") }
	).

io__open_output(FileName, Result) -->
	io__do_open_output(FileName, Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't open output file") }
	).

io__open_append(FileName, Result) -->
	io__do_open_append(FileName, Result0, NewStream),
	( { Result0 \= -1 } ->
		{ Result = ok(NewStream) },
		io__insert_stream_name(NewStream, FileName)
	;
		% XXX improve error message
		{ Result = error("can't append to file") }
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

%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's tell/1 and told/0.

io__told -->
	io__stdout_stream(Stdout),
	io__set_output_stream(Stdout, OldStream),
	io__close_output(OldStream).

io__tell(File, Result) -->
	io__open_output(File, Result0),
	( { Result0 = ok(Stream) } ->
		io__set_output_stream(Stream, _),
		{ Result = ok }
	;
		% XXX improve error message
		{ Result = error("can't open output file") }
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

:- pred io__stream_name(io__stream, string, io__state, io__state).
:- mode io__stream_name(in, out, di, uo) is det.

	% XXX major design flaw with regard to unique modes
	% means that this is very inefficient.
io__stream_name(Stream, Name, IOState0, IOState) :-
	IOState0 = io__state(StreamNames0, B, C, D, E),
	copy(StreamNames0, StreamNames),
	IOState = io__state(StreamNames, B, C, D, E),
	( map__search(StreamNames0, Stream, Name1) ->
		Name = Name1
	;
		Name = "<stream name unavailable>"
	).

:- pred io__delete_stream_name(io__stream, io__state, io__state).
:- mode io__delete_stream_name(in, di, uo) is det.

io__delete_stream_name(Stream, io__state(StreamNames0, B, C, D, E),
		io__state(StreamNames, B, C, D, E)) :-
	map__delete(StreamNames0, Stream, StreamNames).

:- pred io__insert_stream_name(io__stream, string, io__state, io__state).
:- mode io__insert_stream_name(in, in, di, uo) is det.

io__insert_stream_name(Stream, Name,
		io__state(StreamNames0, B, C, D, E),
		io__state(StreamNames, B, C, D, E)) :-
	copy(Stream, Stream1),
	copy(Name, Name1),
	map__set(StreamNames0, Stream1, Name1, StreamNames).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% global state predicates

	% XXX major design flaw with regard to unique modes
	% and io__get_globals/3

/* old definition
io__get_globals(Globals, IOState, IOState) :-
	IOState = io__state(_, _, _, Globals, _).
*/
/* new definition - horrendously inefficient! */
io__get_globals(Globals, IOState0, IOState) :-
	IOState0 = io__state(A, B, C, Globals0, E),
	copy(Globals0, Globals1),
	IOState = io__state(A, B, C, Globals1, E),
	Globals = Globals0.

io__set_globals(Globals, io__state(A, B, C, _, E),
		io__state(A, B, C, Globals, E)).

io__progname_base(DefaultName, PrognameBase) -->
	io__progname(DefaultName, Progname),
	{ dir__basename(Progname, PrognameBase) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% environment interface predicates

io__get_environment_var(Var, OptValue) -->
	( { io__getenv(Var, Value) } ->
	    { OptValue0 = yes(Value) }
	;
	    { OptValue0 = no }
	),
	{ OptValue = OptValue0 }.

io__set_environment_var(Var, Value) -->
	{ string__format("%s=%s", [s(Var), s(Value)], EnvString) },
	( { io__putenv(EnvString) } ->
	    []
	;
	    % XXX What is good behaviour here?

	    { string__format("Could not set environment variable %s",
				[s(Var)], Message) },
	    { error(Message) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% memory management predicates

io__report_stats -->
	{ report_stats }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% miscellaneous predicates

io__init_state(ExternalState, IOState) :-
	map__init(Names0),
	map__init(PutBack),
	ops__init_op_table(OpTable),
	type_to_univ("<globals>", Globals),
	IOState0 = io__state(Names0, PutBack, OpTable, Globals, ExternalState),
	io__insert_std_stream_names(IOState0, IOState).

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
	{ Status = -1 ->
		% XXX improve error message
		Result = error("can't invoke system command")
	;
		Result = ok(Status)
	}.

:- type io__error	==	string.		% This is subject to change.

io__error_message(Error, Error).

%-----------------------------------------------------------------------------%

	% XXX major design flaw with regard to unique modes and
	% io__get_op_table
/* old definition
io__get_op_table(OpTable) -->
	=(io__state(_, _, OpTable, _, _)).
*/
/* new definition - awfully inefficient! */
io__get_op_table(OpTable, IOState0, IOState) :-
	IOState0 = io__state(A, B, OpTable, D, E),
	copy(OpTable, OpTable1),
	IOState = io__state(A, B, OpTable1, D, E).

io__set_op_table(OpTable,	io__state(A, B, _, D, E),
				io__state(A, B, OpTable, D, E)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
