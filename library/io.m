%-----------------------------------------------------------------------------r
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------r
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: io.m.
% Main author: fjh.
% Stability: medium to high.
%
% This file encapsulates all the file I/O.
%
% We implement a purely logical I/O system using non-logical I/O primitives
% of the underlying system (C, Java or IL).
% We ensure referential transparency by passing around a ``state-of-the-world''
% argument using unique modes. The compiler will check that the state of the
% world argument is properly single-threaded, and will also ensure that you
% don't attempt to backtrack over any I/O.
%
% Attempting any operation on a stream which has already been closed results
% in undefined behaviour.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module deconstruct.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.
:- import_module time.
:- import_module univ.

%-----------------------------------------------------------------------------%
%
% Exported types
%
    % The state of the universe.
    %
:- type io.state.

    % An alternative, more concise name for `io.state'.
    %
:- type io.io == io.state.

    % Opaque handles for text I/O streams.
    %
:- type io.input_stream.

:- type io.output_stream.

    % Opaque handles for binary I/O streams.
    %
:- type io.binary_input_stream     ==  io.binary_stream.

:- type io.binary_output_stream    ==  io.binary_stream.

:- type io.binary_stream.

    % A unique identifier for an IO stream.
    %
:- type io.stream_id.

    % Various types used for the result from the access predicates.
    %
:- type io.res
    --->    ok
    ;       error(io.error).

:- type io.res(T)
    --->    ok(T)
    ;       error(io.error).

    % io.maybe_partial_res is used where it is possible to return
    % a partial result when an error occurs,
:- type io.maybe_partial_res(T)
    --->    ok(T)
    ;       error(T, io.error).

:- type io.result
    --->    ok
    ;       eof
    ;       error(io.error).

:- type io.result(T)
    --->    ok(T)
    ;       eof
    ;       error(io.error).

:- type io.read_result(T)
    --->    ok(T)
    ;       eof
    ;       error(string, int). % error message, line number

:- type io.error.   % Use io.error_message to decode it.

    % Poly-type is used for io.write_many and io.format,
    % which do printf-like formatting.

:- type io.poly_type == string.poly_type.

    % io.whence denotes the base for a seek operation.
    %   set - seek relative to the start of the file
    %   cur - seek relative to the current position in the file
    %   end - seek relative to the end of the file.

:- type io.whence
    --->    set
    ;       cur
    ;       end.

%-----------------------------------------------------------------------------%
%
% Text input predicates.
%

    % Reads a character from the current input stream.
    %
:- pred io.read_char(io.result(char)::out, io::di, io::uo) is det.

    % Reads a whitespace delimited word from the current input stream.
    %
:- pred io.read_word(io.result(list(char))::out, io::di, io::uo) is det.

    % Reads a line from the current input stream, returns the result
    % as a list of chars.
    %
:- pred io.read_line(io.result(list(char))::out, io::di, io::uo) is det.

    % Reads a line from the current input stream, returns the result
    % as a string.
    %
:- pred io.read_line_as_string(io.result(string)::out, io::di, io::uo) is det.

    % Reads all the characters from the current input stream until
    % eof or error.
    %
:- pred io.read_file(io.maybe_partial_res(list(char))::out, io::di, io::uo)
    is det.

    % Reads all the characters from the current input stream until
    % eof or error. Returns the result as a string rather than
    % as a list of char.
    %
:- pred io.read_file_as_string(io.maybe_partial_res(string)::out,
    io::di, io::uo) is det.

    % Applies the given closure to each character read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl(pred(char, T, T), T, io.maybe_partial_res(T),
    io, io).
:- mode io.input_stream_foldl((pred(in, in, out) is det), in, out,
    di, uo) is det.
:- mode io.input_stream_foldl((pred(in, in, out) is cc_multi), in, out,
    di, uo) is cc_multi.

    % Applies the given closure to each character read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl_io(pred(char, io, io), io.res, io, io).
:- mode io.input_stream_foldl_io((pred(in, di, uo) is det), out, di, uo)
    is det.
:- mode io.input_stream_foldl_io((pred(in, di, uo) is cc_multi), out, di, uo)
    is cc_multi.

    % Applies the given closure to each character read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl2_io(pred(char, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl2_io((pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl2_io((pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character read from the input stream
    % in turn, until eof or error, or the closure returns `no' as its
    % second argument.
    %
:- pred io.input_stream_foldl2_io_maybe_stop(
    pred(char, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Un-reads a character from the current input stream.
    % You can put back as many characters as you like.
    % You can even put back something that you didn't actually read.
    % Note: `io.putback_char' uses the C library function ungetc().
    % On some systems only one character of pushback is guaranteed.
    % `io.putback_char' will throw an io.error exception if ungetc() fails.
    %
:- pred io.putback_char(char::in, io::di, io::uo) is det.

    % Reads a character from specified stream.
    %
:- pred io.read_char(io.input_stream::in, io.result(char)::out,
    io::di, io::uo) is det.

    % Reads a whitespace delimited word from specified stream.
    %
:- pred io.read_word(io.input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Reads a line from specified stream, returning the result
    % as a list of chars.
    %
:- pred io.read_line(io.input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Reads a line from specified stream, returning the
    % result as a string.
    %
:- pred io.read_line_as_string(io.input_stream::in, io.result(string)::out,
    io::di, io::uo) is det.

    % Reads all the characters from the given input stream until
    % eof or error.
    %
:- pred io.read_file(io.input_stream::in,
    io.maybe_partial_res(list(char))::out, io::di, io::uo) is det.

    % Reads all the characters from the given input stream until eof or error.
    % Returns the result as a string rather than as a list of char.
    %
:- pred io.read_file_as_string(io.input_stream::in,
    io.maybe_partial_res(string)::out, io::di, io::uo) is det.

    % Applies the given closure to each character read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl(io.input_stream, pred(char, T, T),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl_io(io.input_stream, pred(char, io, io),
    io.res, io, io).
:- mode io.input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode io.input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each character read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl2_io(io.input_stream,
    pred(char, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl2_io(in,
    in(pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl2_io(in,
    in(pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character read from the input stream
    % in turn, until eof or error, or the closure returns `no' as its
    % second argument.
    %
:- pred io.input_stream_foldl2_io_maybe_stop(io.input_stream,
    pred(char, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Un-reads a character from specified stream.
    % You can put back as many characters as you like.
    % You can even put back something that you didn't actually read.
    % Note: `io.putback_char' uses the C library function ungetc().
    % On some systems only one character of pushback is guaranteed.
    % `io.putback_char' will throw an io.error exception if ungetc() fails.
    %
:- pred io.putback_char(io.input_stream::in, char::in, io::di, io::uo) is det.

    % Reads a ground term of any type, written using standard Mercury syntax,
    % from the current or specified input stream. The type of the term read
    % is determined by the context in which `io.read' is used.
    %
    % First, the input stream is read until an end-of-term token, end-of-file,
    % or I/O error is reached.  (An end-of-term token consists of a `.'
    % followed by whitespace. The trailing whitespace is left in the input
    % stream.)
    %
    % Then, the result is determined according to the tokens read. If there
    % were no non-whitespace characters before the end of file, then `io.read'
    % returns `eof'. If the tokens read formed a syntactically correct ground
    % term of the correct type, followed by an end-of-term token, then it
    % returns `ok(Term)'. If characters read from the input stream did not form
    % a syntactically correct term, or if the term read is not a ground term,
    % or if the term is not a valid term of the appropriate type, or if an
    % I/O error is encountered, then it returns `error(Message, LineNumber)'.
    %
:- pred io.read(io.read_result(T)::out, io::di, io::uo) is det.
:- pred io.read(io.input_stream::in, io.read_result(T)::out, io::di, io::uo)
    is det.

    % The type `posn' represents a position within a string.
:- type posn
    --->    posn(int, int, int).
            % line number, offset of start of line, current offset (the first
            % two are used only for the purposes of computing term_contexts,
            % for use e.g. in error messages). Offsets start at zero.

    % io.read_from_string(FileName, String, MaxPos, Result, Posn0, Posn):
    % Same as io.read/4 except that it reads from a string rather than
    % from a stream.
    % FileName is the name of the source (for use in error messages).
    % String is the string to be parsed.
    % Posn0 is the position to start parsing from.
    % Posn is the position one past where the term read in ends.
    % MaxPos is the offset in the string which should be considered the
    % end-of-stream -- this is the upper bound for Posn. (In the usual case,
    % MaxPos is just the length of the String.)
    % WARNING: if MaxPos > length of String then the behaviour is UNDEFINED.
    %
:- pred io.read_from_string(string::in, string::in, int::in,
    io.read_result(T)::out, posn::in, posn::out) is det.

    % Discards all the whitespace from the current stream.
    %
:- pred io.ignore_whitespace(io.result::out, io::di, io::uo) is det.

    % Discards all the whitespace from the specified stream.
    %
:- pred io.ignore_whitespace(io.input_stream::in, io.result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Text output predicates.
%

% These will all throw an io.error exception if an I/O error occurs.

    % io.print/3 writes its argument to the standard output stream.
    % io.print/4 writes its second argument to the output stream specified
    % in its first argument. In all cases, the argument to output can be
    % of any type. It is output in a format that is intended to be human
    % readable.
    %
    % If the argument is just a single string or character, it will be printed
    % out exactly as is (unquoted). If the argument is of type univ, then
    % it will print out the value stored in the univ, but not the type.
    %
    % io.print/5 is the same as io.print/4 except that it allows the caller
    % to specify how non-canonical types should be handled. io.print/3 and
    % io.print/4 implicitly specify `canonicalize' as the method for handling
    % non-canonical types. This means that for higher-order types, or types
    % with user-defined equality axioms, or types defined using the foreign
    % language interface (i.e. c_pointer type or pragma foreign_type),
    % the text output will only describe the type that is being printed,
    % not the value.
    %
    % io.print_cc/3 is the same as io.print/3 except that it specifies
    % `include_details_cc' rather than `canonicalize'. This means that it will
    % print the details of non-canonical types. However, it has determinism
    % `cc_multi'.
    %
    % Note that even if `include_details_cc' is specified, some implementations
    % may not be able to print all the details for higher-order types or types
    % defined using the foreign language interface.
    %
:- pred io.print(T::in, io::di, io::uo) is det.

:- pred io.print(io.output_stream::in, T::in, io::di, io::uo) is det.

:- pred io.print(io.output_stream, deconstruct.noncanon_handling, T,
    io, io).
:- mode io.print(in, in(do_not_allow), in, di, uo) is det.
:- mode io.print(in, in(canonicalize), in, di, uo) is det.
:- mode io.print(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.print(in, in, in, di, uo) is cc_multi.

:- pred io.print_cc(T::in, io::di, io::uo) is cc_multi.

    % io.write/3 writes its argument to the current output stream.
    % io.write/4 writes its second argument to the output stream specified
    % in its first argument. In all cases, the argument to output may be
    % of any type. The argument is written in a format that is intended to
    % be valid Mercury syntax whenever possible.
    %
    % Strings and characters are always printed out in quotes, using backslash
    % escapes if necessary. For higher-order types, or for types defined
    % using the foreign language interface (pragma foreign_code), the text
    % output will only describe the type that is being printed, not the value,
    % and the result may not be parsable by `io.read'. For the types
    % containing existential quantifiers, the type `type_desc' and closure
    % types, the result may not be parsable by `io.read', either. But in all
    % other cases the format used is standard Mercury syntax, and if you append
    % a period and newline (".\n"), then the results can be read in again
    % using `io.read'.
    %
    % io.write/5 is the same as io.write/4 except that it allows the caller
    % to specify how non-canonical types should be handled. io.write_cc/3
    % is the same as io.write/3 except that it specifies `include_details_cc'
    % rather than `canonicalize'.
    %
:- pred io.write(T::in, io::di, io::uo) is det.

:- pred io.write(io.output_stream::in, T::in, io::di, io::uo) is det.

:- pred io.write(io.output_stream, deconstruct.noncanon_handling, T, io, io).
:- mode io.write(in, in(do_not_allow), in, di, uo) is det.
:- mode io.write(in, in(canonicalize), in, di, uo) is det.
:- mode io.write(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.write(in, in, in, di, uo) is cc_multi.

:- pred io.write_cc(T::in, io::di, io::uo) is cc_multi.

    % Writes a newline character to the current output stream.
    %
:- pred io.nl(io::di, io::uo) is det.

    % Writes a newline character to the specified output stream.
    %
:- pred io.nl(io.output_stream::in, io::di, io::uo) is det.

    % Writes a string to the current output stream.
    %
:- pred io.write_string(string::in, io::di, io::uo) is det.

    % Writes a string to the specified output stream.
    %
:- pred io.write_string(io.output_stream::in, string::in, io::di, io::uo)
    is det.

    % Writes a list of strings to the current output stream.
    %
:- pred io.write_strings(list(string)::in, io::di, io::uo) is det.

    % Writes a list of strings to the specified output stream.
    %
:- pred io.write_strings(io.output_stream::in, list(string)::in,
    io::di, io::uo) is det.

    % Writes a character to the current output stream.
    %
:- pred io.write_char(char::in, io::di, io::uo) is det.

    % Writes a character to the specified output stream.
    %
:- pred io.write_char(io.output_stream::in, char::in, io::di, io::uo) is det.

    % Writes an integer to the current output stream.
    %
:- pred io.write_int(int::in, io::di, io::uo) is det.

    % Writes an integer to the specified output stream.
    %
:- pred io.write_int(io.output_stream::in, int::in, io::di, io::uo) is det.

    % Writes a floating point number to the current output stream.
    %
:- pred io.write_float(float::in, io::di, io::uo) is det.

    % Writes a floating point number to the specified output stream.
    %
:- pred io.write_float(io.output_stream::in, float::in, io::di, io::uo)
    is det.

    % Formats the specified arguments according to the format string,
    % using string.format, and then writes the result to the current
    % output stream. (See the documentation of string.format for details.)
    %
:- pred io.format(string::in, list(io.poly_type)::in, io::di, io::uo) is det.

    % Formats the specified argument list according to the format string,
    % using string.format, and then writes the result to the specified
    % output stream. (See the documentation of string.format for details.)
    %
:- pred io.format(io.output_stream::in, string::in, list(io.poly_type)::in,
    io::di, io::uo) is det.

    % Writes the specified arguments to the current output stream.
    %
:- pred io.write_many(list(io.poly_type)::in, io::di, io::uo) is det.

    % Writes the specified arguments to the specified output stream.
    %
:- pred io.write_many(io.output_stream::in, list(io.poly_type)::in,
    io::di, io::uo) is det.

    % io.write_list(List, Separator, OutputPred, !IO):
    % applies OutputPred to each element of List, printing Separator
    % between each element. Outputs to the current output stream.
    %
:- pred io.write_list(list(T), string, pred(T, io, io), io, io).
:- mode io.write_list(in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode io.write_list(in, in, pred(in, di, uo) is cc_multi, di, uo)
    is cc_multi.

    % io.write_list(Stream, List, Separator, OutputPred, !IO):
    % applies OutputPred to each element of List, printing Separator
    % between each element. Outputs to Stream.
    %
:- pred io.write_list(io.output_stream, list(T), string,
    pred(T, io, io), io, io).
:- mode io.write_list(in, in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode io.write_list(in, in, in, pred(in, di, uo) is cc_multi, di, uo)
    is cc_multi.

    % Flush the output buffer of the current output stream.
    %
:- pred io.flush_output(io::di, io::uo) is det.

    % Flush the output buffer of the specified output stream.
    %
:- pred io.flush_output(io.output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Input text stream predicates.
%

    % io.see(File, Result, !IO):
    % Attempts to open a file for input, and if successful,
    % sets the current input stream to the newly opened stream.
    % Result is either 'ok' or 'error'.
    %
:- pred io.see(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current input stream.
    % The current input stream reverts to standard input.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred io.seen(io::di, io::uo) is det.

    % Attempts to open a file for input.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred io.open_input(string::in, io.res(io.input_stream)::out,
    io::di, io::uo) is det.

    % Closes an open input stream. This will throw an io.error exception
    % if an I/O error occurs.
    %
:- pred io.close_input(io.input_stream::in, io::di, io::uo) is det.

    % Retrieves the current input stream. Does not modify the IO state.
    %
:- pred io.input_stream(io.input_stream::out, io::di, io::uo) is det.

    % io.set_input_stream(NewStream, OldStream, !IO):
    % Changes the current input stream to the stream specified.
    % Returns the previous stream.
    %
:- pred io.set_input_stream(io.input_stream::in, io.input_stream::out,
    io::di, io::uo) is det.

    % Retrieves the standard input stream.
    %
:- func io.stdin_stream = io.input_stream.

    % Retrieves the standard input stream. Does not modify the IO state.
    %
:- pred io.stdin_stream(io.input_stream::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current input
    % stream. For file streams, this is the filename. For stdin,
    % this is the string "<standard input>".
    %
:- pred io.input_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified input
    % stream. For file streams, this is the filename. For stdin,
    % this is the string "<standard input>".
    %
:- pred io.input_stream_name(io.input_stream::in, string::out,
    io::di, io::uo) is det.

    % Return the line number of the current input stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % io.set_line_number.
    %
:- pred io.get_line_number(int::out, io::di, io::uo) is det.

    % Return the line number of the specified input stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % io.set_line_number.
    %
:- pred io.get_line_number(io.input_stream::in, int::out, io::di, io::uo)
    is det.

    % Set the line number of the current input stream.
    %
:- pred io.set_line_number(int::in, io::di, io::uo) is det.

    % Set the line number of the specified input stream.
    %
:- pred io.set_line_number(io.input_stream::in, int::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%
% Output text stream predicates.
%

    % Attempts to open a file for output, and if successful sets the current
    % output stream to the newly opened stream. As per Prolog tell/1.
    % Result is either 'ok' or 'error(ErrCode)'.
    %
:- pred io.tell(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current output stream; the default output stream reverts
    % to standard output. As per Prolog told/0. This will throw an
    % io.error exception if an I/O error occurs.
    %
:- pred io.told(io::di, io::uo) is det.

    % Attempts to open a file for output.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred io.open_output(string::in, io.res(io.output_stream)::out,
    io::di, io::uo) is det.

    % Attempts to open a file for appending.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred io.open_append(string::in, io.res(io.output_stream)::out,
    io::di, io::uo) is det.

    % Closes an open output stream.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred io.close_output(io.output_stream::in, io::di, io::uo) is det.

    % Retrieves the current output stream. Does not modify the IO state.
    %
:- pred io.output_stream(io.output_stream::out, io::di, io::uo) is det.

    % Changes the current output stream to the stream specified.
    % Returns the previous stream.
    %
:- pred io.set_output_stream(io.output_stream::in, io.output_stream::out,
    io::di, io::uo) is det.

    % Retrieves the standard output stream.
    %
:- func io.stdout_stream = io.output_stream.

    % Retrieves the standard output stream. Does not modify the IO state.
    %
:- pred io.stdout_stream(io.output_stream::out, io::di, io::uo) is det.
    
    % Retrieves the standard error stream.
    %
:- func io.stderr_stream = io.output_stream.

    % Retrieves the standard error stream. Does not modify the IO state.
    %
:- pred io.stderr_stream(io.output_stream::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current
    % output stream.
    % For file streams, this is the filename.
    % For stdout this is the string "<standard output>".
    % For stderr this is the string "<standard error>".
    %
:- pred io.output_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified stream.
    % For file streams, this is the filename.
    % For stdout this is the string "<standard output>".
    % For stderr this is the string "<standard error>".
    %
:- pred io.output_stream_name(io.output_stream::in, string::out,
    io::di, io::uo) is det.

    % Return the line number of the current output stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % io.set_output_line_number.
    %
:- pred io.get_output_line_number(int::out, io::di, io::uo) is det.

    % Return the line number of the specified output stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % io.set_output_line_number.
    %
:- pred io.get_output_line_number(io.output_stream::in, int::out,
    io::di, io::uo) is det.

    % Set the line number of the current output stream.
    %
:- pred io.set_output_line_number(int::in, io::di, io::uo) is det.

    % Set the line number of the specified output stream.
    %
:- pred io.set_output_line_number(io.output_stream::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Binary input predicates.
%

    % Reads a binary representation of a term of type T from the current
    % binary input stream.
    %
:- pred io.read_binary(io.result(T)::out, io::di, io::uo) is det.

    % Reads a binary representation of a term of type T from the specified
    % binary input stream.
    %
    % Note: if you attempt to read a binary representation written by a
    % different program, or a different version of the same program,
    % then the results are not guaranteed to be meaningful. Another caveat
    % is that higher-order types cannot be read. (If you try, you will get
    % a runtime error.)
    %
    % XXX Note also that due to the current implementation,
    % io.read_binary will not work for the Java back-end.
    %
:- pred io.read_binary(io.binary_input_stream::in, io.result(T)::out,
    io::di, io::uo) is det.

    % Reads a single 8-bit byte from the current binary input stream.
    %
:- pred io.read_byte(io.result(int)::out, io::di, io::uo) is det.

    % Reads a single 8-bit byte from the specified binary input stream.
    %
:- pred io.read_byte(io.binary_input_stream::in, io.result(int)::out,
    io::di, io::uo) is det.

    % Reads all the bytes from the current binary input stream
    % until eof or error.
    %
:- pred io.read_binary_file(io.result(list(int))::out, io::di, io::uo) is det.

    % Reads all the bytes from the given binary input stream until
    % eof or error.
    %
:- pred io.read_binary_file(io.input_stream::in, io.result(list(int))::out,
    io::di, io::uo) is det.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred io.binary_input_stream_foldl(pred(int, T, T),
    T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl((pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl((pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred io.binary_input_stream_foldl_io(pred(int, io, io),
    io.res, io, io).
:- mode io.binary_input_stream_foldl_io((pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode io.binary_input_stream_foldl_io((pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred io.binary_input_stream_foldl2_io(
    pred(int, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io(
    in(pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io(
    in(pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error, or the closure returns `no'
    % as its second argument.
    %
:- pred io.binary_input_stream_foldl2_io_maybe_stop(
    pred(int, bool, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred io.binary_input_stream_foldl(io.binary_input_stream,
    pred(int, T, T), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred io.binary_input_stream_foldl_io(io.binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode io.binary_input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode io.binary_input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred io.binary_input_stream_foldl2_io(io.binary_input_stream,
    pred(int, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the
    % given binary input stream in turn, until eof or error,
    % or the closure returns `no' as its second argument.
    %
:- pred io.binary_input_stream_foldl2_io_maybe_stop(io.binary_input_stream,
    pred(int, bool, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Un-reads a byte from the current binary input stream.
    % You can put back as many bytes as you like.
    % You can even put back something that you didn't actually read.
    % The byte is taken from the bottom 8 bits of an integer.
    % Note: `io.putback_byte' uses the C library function ungetc().
    % On some systems only one byte of pushback is guaranteed.
    % `io.putback_byte' will throw an io.error exception if ungetc() fails.
    %
:- pred io.putback_byte(int::in, io::di, io::uo) is det.

    % Un-reads a byte from specified binary input stream.
    % You can put back as many bytes as you like.
    % You can even put back something that you didn't actually read.
    % The byte is returned in the bottom 8 bits of an integer.
    % Note: `io.putback_byte' uses the C library function ungetc().
    % On some systems only one byte of pushback is guaranteed.
    % `io.putback_byte' will throw an io.error exception if ungetc() fails.
    %
:- pred io.putback_byte(io.binary_input_stream::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Binary output predicates.
%

% These will all throw an io.error exception if an I/O error occurs.
% XXX what about wide characters?

    % Writes a binary representation of a term to the current binary output
    % stream, in a format suitable for reading in again with io.read_binary.
    %
:- pred io.write_binary(T::in, io::di, io::uo) is det.

    % Writes a binary representation of a term to the specified binary output
    % stream, in a format suitable for reading in again with io.read_binary.
    %
    % XXX Note that due to the current implementation, io.write_binary
    % will not work for the Java back-end.
    %
:- pred io.write_binary(io.binary_output_stream::in, T::in, io::di, io::uo)
    is det.

    % Writes a single byte to the current binary output stream.
    % The byte is taken from the bottom 8 bits of an int.
    %
:- pred io.write_byte(int::in, io::di, io::uo) is det.

    % Writes a single byte to the specified binary output stream.
    % The byte is taken from the bottom 8 bits of an int.
    %
:- pred io.write_byte(io.binary_output_stream::in, int::in, io::di, io::uo)
    is det.

    % Writes several bytes to the current binary output stream.
    % The bytes are taken from a string.
    %
:- pred io.write_bytes(string::in, io::di, io::uo) is det.

    % Writes several bytes to the specified binary output stream.
    % The bytes are taken from a string.
    %
:- pred io.write_bytes(io.binary_output_stream::in, string::in,
    io::di, io::uo) is det.

    % Flush the output buffer of the current binary output stream.
    %
:- pred io.flush_binary_output(io::di, io::uo) is det.

    % Flush the output buffer of the specified binary output stream.
    %
:- pred io.flush_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
:- pred io.seek_binary(io.binary_stream::in, io.whence::in, int::in,
    io::di, io::uo) is det.

    % Returns the offset (in bytes) into the specified binary stream.
    %
:- pred io.binary_stream_offset(io.binary_stream::in, int::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Binary input stream predicates.
%

    % Attempts to open a file for binary input, and if successful sets
    % the current binary input stream to the newly opened stream.
    % Result is either 'ok' or 'error'.
    %
:- pred io.see_binary(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current input stream. The current input stream reverts
    % to standard input. This will throw an io.error exception if
    % an I/O error occurs.
    %
:- pred io.seen_binary(io::di, io::uo) is det.

    % Attempts to open a binary file for input.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred io.open_binary_input(string::in,
    io.res(io.binary_input_stream)::out, io::di, io::uo) is det.

    % Closes an open binary input stream. This will throw an io.error
    % exception if an I/O error occurs.
    %
:- pred io.close_binary_input(io.binary_input_stream::in,
    io::di, io::uo) is det.

    % Retrieves the current binary input stream.
    % Does not modify the IO state.
    %
:- pred io.binary_input_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

    % Changes the current input stream to the stream specified.
    % Returns the previous stream.
    %
:- pred io.set_binary_input_stream(io.binary_input_stream::in,
    io.binary_input_stream::out, io::di, io::uo) is det.

    % Retrieves the standard binary input stream.
    % Does not modify the IO state.
    %
:- pred io.stdin_binary_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current binary
    % input stream. For file streams, this is the filename.
    %
:- pred io.binary_input_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified
    % binary input stream. For file streams, this is the filename.
    %
:- pred io.binary_input_stream_name(io.binary_input_stream::in, string::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Binary output stream predicates.
%

    % Attempts to open a file for binary output, and if successful sets
    % the current binary output stream to the newly opened stream.
    % As per Prolog tell/1. Result is either 'ok' or 'error(ErrCode)'.
    %
:- pred io.tell_binary(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current binary output stream. The default binary output
    % stream reverts to standard output. As per Prolog told/0. This will
    % throw an io.error exception if an I/O error occurs.
    %
:- pred io.told_binary(io::di, io::uo) is det.

    % Attempts to open a file for binary output.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred io.open_binary_output(string::in,
    io.res(io.binary_output_stream)::out, io::di, io::uo) is det.

    % Attempts to open a file for binary appending.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred io.open_binary_append(string::in,
    io.res(io.binary_output_stream)::out, io::di, io::uo) is det.

    % Closes an open binary output stream.
    % This will throw an io.error exception
    % if an I/O error occurs.
    %
:- pred io.close_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

    % Retrieves the current binary output stream.
    % Does not modify the IO state.
    %
:- pred io.binary_output_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

    % Retrieves the standard binary output stream.
    % Does not modify the IO state.
    %
:- pred io.stdout_binary_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

    % Changes the current binary output stream to the stream specified.
    % Returns the previous stream.
    %
:- pred io.set_binary_output_stream(io.binary_output_stream::in,
    io.binary_output_stream::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current
    % binary output stream. For file streams, this is the filename.
    %
:- pred io.binary_output_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified
    % output stream. For file streams, this is the filename.
    %
:- pred io.binary_output_stream_name(io.binary_output_stream::in,
    string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Global state predicates.
%

    % io.progname(DefaultProgname, Progname):
    %
    % Returns the name that the program was invoked with, if available,
    % or DefaultProgname if the name is not available.
    % Does not modify the IO state.
    %
:- pred io.progname(string::in, string::out, io::di, io::uo) is det.

    % io.progname_base(DefaultProgname, Progname):
    %
    % Like `io.progname', except that it strips off any path name
    % preceding the program name.  Useful for error messages.
    %
:- pred io.progname_base(string::in, string::out, io::di, io::uo) is det.

    % Returns the arguments that the program was invoked with,
    % if available, otherwise an empty list. Does not modify the IO state.
    %
:- pred io.command_line_arguments(list(string)::out, io::di, io::uo) is det.

    % The io.state contains an integer used to record the program's exit
    % status. When the program finishes, it will return this exit status
    % to the operating system. The following predicates can be used to get
    % and set the exit status.
    %
:- pred io.get_exit_status(int::out, io::di, io::uo) is det.

:- pred io.set_exit_status(int::in, io::di, io::uo) is det.

    % The io.state includes a `globals' field which is not used by the I/O
    % library, but can be used by the application. The globals field is
    % of type `univ' so that the application can store any data it wants there.
    % The following predicates can be used to access this global state.
    %
    % Doesn't modify the io.state.
    %
:- pred io.get_globals(univ::uo, io::di, io::uo) is det.

:- pred io.set_globals(univ::di, io::di, io::uo) is det.

    % The following predicates provide an interface to the environment list.
    % Do not attempt to put spaces or '=' signs in the names of environment
    % variables, or bad things may result!
    %
    % First argument is the name of the environment variable. Returns
    % yes(Value) if the variable was set (Value will be set to the value
    % of the variable) and no if the variable was not set.
    %
:- pred io.get_environment_var(string::in, maybe(string)::out,
    io::di, io::uo) is det.

    % First argument is the name of the environment variable, second argument
    % is the value to be assigned to that variable. Will throw an exception
    % if the system runs out of environment space.
    %
:- pred io.set_environment_var(string::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% File handling predicates
%

    % io.make_temp(Name, !IO) creates an empty file whose name is different
    % to the name of any existing file. Name is bound to the name of the file.
    % It is the responsibility of the program to delete the file when it is
    % no longer needed.
    %
    % The file will reside in an implementation-dependent directory.
    % For current Mercury implementations, it is determined as follows:
    % 1. For the non-Java back-ends:
    %    - On Microsoft Windows systems, the file will reside in
    %      the current directory if the TMP environment variable
    %      is not set, or in the directory specified by TMP if it is set.
    %    - On Unix systems, the file will reside in /tmp if the TMPDIR
    %      environment variable is not set, or in the directory specified
    %      by TMPDIR if it is set.
    % 2. For the Java back-end, the system-dependent default
    %    temporary-file directory will be used, specified by the Java
    %    system property java.io.tmpdir. On UNIX systems the default
    %    value of this property is typically "/tmp" or "/var/tmp";
    %    on Microsoft Windows systems it is typically "c:\\temp".
    %
:- pred io.make_temp(string::out, io::di, io::uo) is det.

    % io.make_temp(Dir, Prefix, Name, IO0, IO) creates an empty file whose
    % name is different to the name of any existing file. The file will reside
    % in the directory specified by `Dir' and will have a prefix using up to
    % the first 5 characters of `Prefix'. Name is bound to the name of the
    % file. It is the responsibility of the program to delete the file
    % when it is no longer needed.
    %
:- pred io.make_temp(string::in, string::in, string::out,
    io::di, io::uo) is det.

    % io.remove_file(FileName, Result, !IO) attempts to remove the file
    % `FileName', binding Result to ok/0 if it succeeds, or error/1 if it
    % fails. If `FileName' names a file that is currently open, the behaviour
    % is implementation-dependent.
    %
:- pred io.remove_file(string::in, io.res::out, io::di, io::uo) is det.

    % io.rename_file(OldFileName, NewFileName, Result, !IO):
    %
    % Attempts to rename the file `OldFileName' as `NewFileName', binding
    % Result to ok/0 if it succeeds, or error/1 if it fails. If `OldFileName'
    % names a file that is currently open, the behaviour is
    % implementation-dependent. If `NewFileName' names a file that already
    % exists the behaviour is also implementation-dependent; on some systems,
    % the file previously named `NewFileName' will be deleted and replaced
    % with the file previously named `OldFileName'.
    %
:- pred io.rename_file(string::in, string::in, io.res::out,
    io::di, io::uo) is det.

    % Can this platform read and create symbolic links.
    %
:- pred io.have_symlinks is semidet.

    % io.make_symlink(FileName, LinkFileName, Result, !IO):
    %
    % Attempts to make `LinkFileName' be a symbolic link to `FileName'.
    % If `FileName' is a relative path, it is interpreted relative
    % to the directory containing `LinkFileName'.
    %
:- pred io.make_symlink(string::in, string::in, io.res::out,
    io::di, io::uo) is det.

    % io.read_symlink(FileName, Result, !IO) returns `ok(LinkTarget)'
    % if `FileName' is a symbolic link pointing to `LinkTarget', and
    % `error(Error)' otherwise. If `LinkTarget' is a relative path,
    % it should be interpreted relative the directory containing `FileName',
    % not the current directory.
    %
:- pred io.read_symlink(string::in, io.res(string)::out, io::di, io::uo)
    is det.

:- type io.access_type
    --->    read
    ;       write
    ;       execute.

    % io.check_file_accessibility(FileName, AccessTypes, Result):
    %
    % Check whether the current process can perform the operations given
    % in `AccessTypes' on `FileName'.
    % XXX When using the .NET CLI, this predicate will sometimes report
    % that a directory is writable when in fact it is not.
    %
:- pred io.check_file_accessibility(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.

:- type io.file_type
    --->    regular_file
    ;       directory
    ;       symbolic_link
    ;       named_pipe
    ;       socket
    ;       character_device
    ;       block_device
    ;       message_queue
    ;       semaphore
    ;       shared_memory
    ;       unknown.

    % io.file_type(FollowSymLinks, FileName, TypeResult)
    % finds the type of the given file.
    %
:- pred io.file_type(bool::in, string::in, io.res(file_type)::out,
    io::di, io::uo) is det.

    % io.file_modification_time(FileName, TimeResult)
    % finds the last modification time of the given file.
    %
:- pred io.file_modification_time(string::in, io.res(time_t)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Memory management predicates.
%

    % Write memory/time usage statistics to stderr.
    %
:- pred io.report_stats(io::di, io::uo) is det.

    % Write statistics to stderr; what statistics will be written
    % is controlled by the first argument, which acts a selector.
    % What selector values cause what statistics to be printed
    % is implementation defined.
    %
    % The Melbourne implementation supports the following selectors:
    %
    % "standard"
    %   Writes memory/time usage statistics.
    %
    % "full_memory_stats"
    %   Writes complete memory usage statistics, including information
    %   about all procedures and types. Requires compilation with memory
    %   profiling enabled.
    %
    % "tabling"
    %   Writes statistics about the internals of the tabling system.
    %   Requires the runtime to have been compiled with the macro
    %   MR_TABLE_STATISTICS defined.
    %
:- pred io.report_stats(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Miscellaneous predicates
%

    % Invokes the operating system shell with the specified Command.
    % Result is either `ok(ExitStatus)', if it was possible to invoke
    % the command, or `error(ErrorCode)' if not. The ExitStatus will be 0
    % if the command completed successfully or the return value of the system
    % call. If a signal kills the system call, then Result will be an error
    % indicating which signal occurred.
    %
:- pred io.call_system(string::in, io.res(int)::out, io::di, io::uo) is det.

:- type io.system_result
    --->    exited(int)
    ;       signalled(int).

    % call_system_return_signal(Command, Result, !IO):
    %
    % Invokes the operating system shell with the specified Command.
    % Result is either `ok(ExitStatus)' if it was possible to invoke
    % the command or `error(Error)' if the command could not be executed.
    % If the command could be executed then ExitStatus is either
    % `exited(ExitCode)' if the command ran to completion or
    % `signalled(SignalNum)' if the command was killed by a signal.
    % If the command ran to completion then ExitCode will be 0 if the command
    % ran successfully and the return value of the command otherwise.
    %
:- pred io.call_system_return_signal(string::in,
    io.res(io.system_result)::out, io::di, io::uo) is det.

    % Construct an error code including the specified error message.
    %
:- func io.make_io_error(string) = io.error.

    % Look up the error message corresponding to a particular error code.
    %
:- func io.error_message(io.error) = string.
:- pred io.error_message(io.error::in, string::out) is det.

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
:- type io.system_error.
:- pragma foreign_type(c, io.system_error, "MR_Integer").
:- pragma foreign_type(il, io.system_error,
    "class [mscorlib]System.Exception").
:- pragma foreign_type(java, io.system_error, "java.lang.Exception").

    % io.make_err_msg(Error, MessagePrefix, Message):
    % `Message' is an error message obtained by looking up the
    % message for the given errno value and prepending
    % `MessagePrefix'.
    %
:- pred io.make_err_msg(io.system_error::in, string::in, string::out,
    io::di, io::uo) is det.

    % Succeeds iff the Win32 API is available.
    %
:- pred have_win32 is semidet.

    % Succeeds iff the current process was compiled against the Cygwin library.
    %
:- pred have_cygwin is semidet.

    % Succeeds iff the .NET class library is available.
    %
:- pred have_dotnet is semidet.

    % io.make_win32_err_msg(Error, MessagePrefix, Message):
    %
    % `Message' is an error message obtained by looking up the
    % error message for the given Win32 error number and prepending
    % `MessagePrefix'.
    % This will abort if called on a system which does not support
    % the Win32 API.
    %
:- pred io.make_win32_err_msg(io.system_error::in,
    string::in, string::out, io::di, io::uo) is det.

    % io.make_maybe_win32_err_msg(Error, MessagePrefix, Message):
    %
    % `Message' is an error message obtained by looking up the
    % last Win32 error message and prepending `MessagePrefix'.
    % On non-Win32 systems, the message corresponding to the
    % current value of errno will be used.
    %
:- pred io.make_maybe_win32_err_msg(io.system_error::in,
    string::in, string::out, io::di, io::uo) is det.

    % Return a unique identifier for the given file (after following
    % symlinks in FileName).
    % XXX On Cygwin sometimes two files will have the same file_id.
    % This is because MS-Windows does not use inodes, so Cygwin hashes
    % the absolute file name. On Windows without Cygwin this will always
    % return error(_). That doesn't matter, because this function is only used
    % for checking for symlink loops in dir.foldl2, but plain Windows
    % doesn't support symlinks.
    %
:- type file_id.
:- pred io.file_id(string::in, io.res(file_id)::out, io::di, io::uo) is det.

    % Succeeds if io.file_id is implemented on this platform.
    %
:- pred io.have_file_ids is semidet.

%
% For use by term_io.m:
%

:- import_module ops.

:- pred io.get_op_table(ops.table::out, io::di, io::uo) is det.

:- pred io.set_op_table(ops.table::di, io::di, io::uo) is det.

:- pred adjust_priority_for_assoc(ops.priority::in, ops.assoc::in,
    ops.priority::out) is det.

:- pred maybe_write_paren(char::in, ops.priority::in, ops.priority::in,
    io::di, io::uo) is det.

%
% For use by browser/browse.m:
%

% Types and predicates for managing the stream info database.

:- type io.stream_db ==    map(io.stream_id, stream_info).

:- type stream_info
    --->    stream(
                stream_id               :: int,
                stream_mode             :: stream_mode,
                stream_content          :: stream_content,
                stream_source           :: stream_source
            ).

:- type maybe_stream_info
    --->    stream(
                maybe_stream_id         :: int,
                maybe_stream_mode       :: stream_mode,
                maybe_stream_content    :: stream_content,
                maybe_stream_source     :: stream_source
            )
    ;       unknown_stream.

:- type stream_mode
    --->    input
    ;       output
    ;       append.

:- type stream_content
    --->    text
    ;       binary
    ;       preopen.

:- type stream_source
    --->    file(string)    % the file name
    ;       stdin
    ;       stdout
    ;       stderr.

    % Retrieves the database mapping streams to the information we have
    % about those streams.
    %
:- pred io.get_stream_db(io.stream_db::out, io::di, io::uo) is det.

    % Returns the information associated with the specified input
    % stream in the given stream database.
    %
:- func io.input_stream_info(io.stream_db, io.input_stream)
    = io.maybe_stream_info.

    % Returns the information associated with the specified output
    % stream in the given stream database.
    %
:- func io.output_stream_info(io.stream_db, io.output_stream)
    = io.maybe_stream_info.

    % Returns the information associated with the specified binary input
    % stream in the given stream database.
    %
:- func io.binary_input_stream_info(io.stream_db, io.binary_input_stream)
    = io.maybe_stream_info.

    % Returns the information associated with the specified binary output
    % stream in the given stream database.
    %
:- func io.binary_output_stream_info(io.stream_db, io.binary_output_stream)
    = io.maybe_stream_info.

% Predicates for writing out univs.

:- pred io.write_univ(univ::in, io::di, io::uo) is det.

:- pred io.write_univ(io.output_stream::in, univ::in, io::di, io::uo) is det.

:- pred io.write_univ(io.output_stream, deconstruct.noncanon_handling, univ,
    io, io).
:- mode io.write_univ(in, in(do_not_allow), in, di, uo) is det.
:- mode io.write_univ(in, in(canonicalize), in, di, uo) is det.
:- mode io.write_univ(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.write_univ(in, in, in, di, uo) is cc_multi.

%
% For use by compiler/process_util.m:
%

    % Interpret the child process exit status returned by
    % system() or wait().
    %
:- func io.handle_system_command_exit_status(int) = io.res(io.system_result).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module benchmarking.
:- import_module bool.
:- import_module dir.
:- import_module enum.
:- import_module exception.
:- import_module int.
:- import_module map.
:- import_module parser.
:- import_module require.
:- import_module term.
:- import_module term_io.
:- import_module type_desc.
:- import_module varset.

:- use_module rtti_implementation.
:- use_module table_builtin.

:- pragma foreign_import_module(c, string).

    % Values of type `io.state' are never really used:
    % instead we store data in global variables.
    % The reason this is not defined simply as `io.state == c_pointer'
    % is so that `type_name' produces more informative results
    % for cases such as `type_name(main)'.
:- type io.state ---> state(c_pointer).

:- pragma foreign_decl("C", "
    extern MR_Word      ML_io_stream_db;
    extern MR_Word      ML_io_user_globals;
    extern int          ML_next_stream_id;
    #if 0
      extern MR_Word    ML_io_ops_table;
    #endif
").

:- pragma foreign_code("C", "
    MR_Word         ML_io_stream_db;
    MR_Word         ML_io_user_globals;
    /* a counter used to generate unique stream ids */
    int             ML_next_stream_id;
    #if 0
      MR_Word       ML_io_ops_table;
    #endif
").

:- pragma foreign_code("C#", "
    // The ML_ prefixes here are not really needed,
    // since the C# code all gets generated inside a class,
    // but we keep them for consistency with the C code.

#if MR_HIGHLEVEL_DATA
    static mercury.tree234.tree234_2    ML_io_stream_db;
    static object[]                     ML_io_user_globals;
#else
    static object[]                     ML_io_stream_db;
    static object[]                     ML_io_user_globals;
#endif

    // a counter used to generate unique stream ids
    static int                          ML_next_stream_id;

    // This specifies the default encoding used for text files.
    // It must be either ML_OS_text_encoding or ML_Unix_text_encoding.
    //
    // XXX The initial setting for this should be controlled
    // by an environment variable.  (This might require moving
    // the code which initializes mercury_stdin, etc.)
    //
    static ML_file_encoding_kind ML_default_text_encoding =
        ML_file_encoding_kind.ML_OS_text_encoding;
").

:- pragma foreign_code("Java",
"
    static java.lang.Object ML_io_stream_db =
        new mercury.tree234.Tree234_2.Empty_0();
    static java.lang.Object ML_io_user_globals =
        new mercury.tree234.Tree234_2.Empty_0();
").

:- type io.stream_putback ==    map(io.stream_id, list(char)).

:- type io.input_stream  ==     io.stream.
:- type io.output_stream ==     io.stream.

:- type io.binary_stream ==     io.stream.

:- type io.stream --->     stream(c_pointer).
:- pragma foreign_type("C", io.stream, "MercuryFilePtr",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("il", io.stream,
    "class [mercury]mercury.io__csharp_code.MR_MercuryFileStruct").
:- pragma foreign_type("Java", io.stream, "mercury.io.MR_MercuryFileStruct").

    % a unique identifier for an IO stream
:- type io.stream_id == int.

:- func io.get_stream_id(io.stream) = io.stream_id.

    % This inter-language stuff is tricky.
    % We communicate via ints rather than via io.result_codes because
    % we don't want the C/Java/etc code to depend on how Mercury stores
    % its discriminated union data types.

    % Reads a character from specified stream, and returns the numerical value
    % for that character (as from char.to_int). This may involve converting
    % external character encodings into Mercury's internal character
    % representation and (for text streams) converting OS line indicators,
    % e.g. CR-LF for Windows, to '\n' characters. Returns -1 if at EOF,
    % -2 if an error occurs.
    %
:- pred io.read_char_code(io.input_stream::in, int::out, io::di, io::uo)
    is det.

    % Reads a byte from specified stream.
    % Returns -1 if at EOF, -2 if an error occurs.
    %
:- pred io.read_byte_val(io.input_stream::in, int::out,
    io::di, io::uo) is det.

    % io.call_system_code(Command, Status, Message, !IO):
    %
    % Invokes the operating system shell with the specified Command.
    % Returns Status = 127 and Message on failure. Otherwise returns
    % the raw exit status from the system() call.
    %
:- pred io.call_system_code(string::in, int::out, string::out,
    io::di, io::uo) is det.

    % io.getenv(Var, Value):
    %
    % Gets the value Value associated with the environment variable Var.
    % Fails if the variable was not set.
    %
:- semipure pred io.getenv(string::in, string::out) is semidet.

    % io.setenv(NameString,ValueString):
    %
    % Sets the named environment variable to the specified value.
    % Fails if the operation does not work.
    %
:- impure pred io.setenv(string::in, string::in) is semidet.

%-----------------------------------------------------------------------------%

% input predicates

% We want to inline these, to allow deforestation.
:- pragma inline(io.read_char/3).
:- pragma inline(io.read_char/4).

io.read_char(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.read_char(Stream, Result, !IO).

io.read_char(Stream, Result, !IO) :-
    io.read_char_code(Stream, Code, !IO),
    ( Code = -1 ->
        Result = eof
    ; char.to_int(Char, Code) ->
        Result = ok(Char)
    ;
        io.make_err_msg("read failed: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

% We want to inline these, to allow deforestation.
:- pragma inline(io.read_byte/3).
:- pragma inline(io.read_byte/4).

io.read_byte(Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_byte(Stream, Result, !IO).

io.read_byte(Stream, Result, !IO) :-
    io.read_byte_val(Stream, Code, !IO),
    ( Code >= 0 ->
        Result = ok(Code)
    ; Code = -1 ->
        Result = eof
    ;
        io.make_err_msg("read failed: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.read_word(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.read_word(Stream, Result, !IO).

io.read_word(Stream, Result, !IO) :-
    io.ignore_whitespace(Stream, WSResult, !IO),
    (
        WSResult = error(Error),
        Result = error(Error)
    ;
        WSResult = eof,
        Result = eof
    ;
        WSResult = ok,
        io.read_word_2(Stream, Result, !IO)
    ).

:- pred io.read_word_2(io.input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

io.read_word_2(Stream, Result, !IO) :-
    io.read_char(Stream, CharResult, !IO),
    (
        CharResult = error(Error),
        Result = error(Error)
    ;
        CharResult = eof,
        Result = eof
    ;
        CharResult = ok(Char),
        ( char.is_whitespace(Char) ->
            io.putback_char(Stream, Char, !IO),
            Result = ok([])
        ;
            io.read_word_2(Stream, Result0, !IO),
            (
                Result0 = ok(Chars),
                Result = ok([Char | Chars])
            ;
                Result0 = error(_),
                Result = Result0
            ;
                Result0 = eof,
                Result = ok([Char])
            )
        )
    ).

io.read_line(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.read_line(Stream, Result, !IO).

io.read_line(Stream, Result, !IO) :-
    io.read_char_code(Stream, Code, !IO),
    ( Code = -1 ->
        Result = eof
    ; char.to_int(Char, Code) ->
        ( Char = '\n' ->
            Result = ok([Char])
        ;
            io.read_line_2(Stream, Result0, !IO),
            Result = ok([Char | Result0])
        )
    ;
        io.make_err_msg("read failed: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

:- pred io.read_line_2(io.input_stream::in, list(char)::out,
    io::di, io::uo) is det.

io.read_line_2(Stream, Result, !IO) :-
    io.read_char_code(Stream, Code, !IO),
    ( Code = -1 ->
        Result = []
    ; char.to_int(Char, Code) ->
        ( Char = '\n' ->
            Result = [Char]
        ;
            io.read_line_2(Stream, Chars, !IO),
            Result = [Char | Chars]
        )
    ;
        Result = []
    ).

io.read_line_as_string(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.read_line_as_string(Stream, Result, !IO).

io.read_line_as_string(Stream, Result, !IO) :-
    io.read_line_as_string_2(Stream, yes, Res, String, !IO),
    ( Res < 0 ->
        ( Res = -1 ->
            Result = eof
        ;
            io.make_err_msg("read failed: ", Msg, !IO),
            Result = error(io_error(Msg))
        )
    ;
        Result = ok(String)
    ).

:- pred io.read_line_as_string_2(io.input_stream::in, bool::in, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.read_line_as_string_2(File::in, _Bool::in, Res :: out,
        RetString::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
#define ML_IO_READ_LINE_GROW(n) ((n) * 3 / 2)
#define ML_IO_BYTES_TO_WORDS(n) (((n) + sizeof(MR_Word) - 1) / sizeof(MR_Word))
#define ML_IO_READ_LINE_START   1024

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
                read_buffer = MR_NEW_ARRAY(MR_Char, read_buf_size);
                MR_memcpy(read_buffer, initial_read_buffer,
                    ML_IO_READ_LINE_START);
            } else {
                read_buffer = MR_RESIZE_ARRAY(read_buffer, MR_Char,
                    read_buf_size);
            }
        }
    }
    if (Res == 0) {
        MR_Word ret_string_word;
        MR_offset_incr_hp_atomic_msg(ret_string_word,
            0, ML_IO_BYTES_TO_WORDS((i + 1) * sizeof(MR_Char)),
            MR_PROC_LABEL, ""string.string/0"");
        RetString = (MR_String) ret_string_word;
        MR_memcpy(RetString, read_buffer, i * sizeof(MR_Char));
        RetString[i] = '\\0';
    } else {
        /*
        ** We can't just return NULL here, because  otherwise mdb will break
        ** when it tries to print the string.
        */
        RetString = MR_make_string_const("""");
    }
    if (read_buffer != initial_read_buffer) {
        MR_free(read_buffer);
    }
    MR_update_io(IO0, IO);
").

io.read_line_as_string_2(Stream, FirstCall, Res, String, !IO) :-
    % XXX This is terribly inefficient, a better approach would be to
    % use a buffer like what is done for io.read_file_as_string.
    io.read_char(Stream, Result, !IO),
    (
        Result = ok(Char),
        ( Char = '\n' ->
            Res = 0,
            String = "\n"
        ;
            io.read_line_as_string_2(Stream, no, Res, String0, !IO),
            string.first_char(String, Char, String0)
        )
    ;
        Result = eof,
        (
            FirstCall = yes,
            String = "",
            Res = -1
        ;
            FirstCall = no,
            String = "",
            Res = 0
        )
    ;
        Result = error(_),
        String = "",
        Res = -2
    ).

io.read_file(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.read_file(Stream, Result, !IO).

io.read_file(Stream, Result, !IO) :-
    io.read_file_2(Stream, [], Result, !IO).

:- pred io.read_file_2(io.input_stream::in, list(char)::in,
    io.maybe_partial_res(list(char))::out, io::di, io::uo) is det.

io.read_file_2(Stream, Chars0, Result, !IO) :-
    io.read_char(Stream, Result0, !IO),
    (
        Result0 = eof,
        Result = ok(list.reverse(Chars0))
    ;
        Result0 = error(Err),
        Result = error(list.reverse(Chars0), Err)
    ;
        Result0 = ok(Char),
        io.read_file_2(Stream, [Char | Chars0], Result, !IO)
    ).

%-----------------------------------------------------------------------------%

io.read_file_as_string(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.read_file_as_string(Stream, Result, !IO).

io.read_file_as_string(Stream, Result, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    io.stream_file_size(Stream, FileSize, !IO),
    ( FileSize >= 0 ->
        BufferSize0 = FileSize + 1
    ;
        BufferSize0 = 4000
    ),
    io.alloc_buffer(BufferSize0, Buffer0),

    % Read the file into the buffer (resizing it as we go if necessary),
    % convert the buffer into a string, and see if anything went wrong.
    io.clear_err(Stream, !IO),
    Pos0 = 0,
    io.read_file_as_string_2(Stream, Buffer0, Buffer, Pos0, Pos,
        BufferSize0, BufferSize, !IO),
    require(Pos < BufferSize, "io.read_file_as_string: overflow"),
    io.buffer_to_string(Buffer, Pos, String),
    io.check_err(Stream, Result0, !IO),
    (
        Result0 = ok,
        Result = ok(String)
    ;
        Result0 = error(Error),
        Result = error(String, Error)
    ).

:- pred io.read_file_as_string_2(io.input_stream::in, buffer::buffer_di,
    buffer::buffer_uo, int::in, int::out, int::in, int::out, io::di, io::uo)
    is det.

io.read_file_as_string_2(Stream, !Buffer, !Pos, !Size, !IO) :-
    Pos0 = !.Pos,
    Size0 = !.Size,
    io.read_into_buffer(Stream, !Buffer, !Pos, !.Size, !IO),
    ( !.Pos =< Pos0 ->
        % End-of-file or error.
        true
    ; !.Pos = Size0 ->
        % Full buffer.
        !:Size = Size0 * 2,
        io.resize_buffer(Size0, !.Size, !Buffer),
        io.read_file_as_string_2(Stream, !Buffer, !Pos, !Size, !IO)
    ;
        io.read_file_as_string_2(Stream, !Buffer, !Pos, !Size, !IO)
    ).

%-----------------------------------------------------------------------------%

io.input_stream_foldl(Pred, T0, Res, !IO) :-
    io.input_stream(Stream, !IO),
    io.input_stream_foldl(Stream, Pred, T0, Res, !IO).

io.input_stream_foldl(Stream, Pred, T0, Res, !IO) :-
    io.read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, T0, T1),
        io.input_stream_foldl(Stream, Pred, T1, Res, !IO)
    ;
        CharResult = eof,
        Res = ok(T0)
    ;
        CharResult = error(Error),
        Res = error(T0, Error)
    ).

io.input_stream_foldl_io(Pred, Res, !IO) :-
    io.input_stream(Stream, !IO),
    io.input_stream_foldl_io(Stream, Pred, Res, !IO).

io.input_stream_foldl_io(Stream, Pred, Res, !IO) :-
    io.read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, !IO),
        io.input_stream_foldl_io(Stream, Pred, Res, !IO)
    ;
        CharResult = eof,
        Res = ok
    ;
        CharResult = error(Error),
        Res = error(Error)
    ).

io.input_stream_foldl2_io(Pred, T0, Res, !IO) :-
    io.input_stream(Stream, !IO),
    io.input_stream_foldl2_io(Stream, Pred, T0, Res, !IO).

io.input_stream_foldl2_io(Stream, Pred, T0, Res, !IO) :-
    io.read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, T0, T1, !IO),
        io.input_stream_foldl2_io(Stream, Pred, T1, Res, !IO)
    ;
        CharResult = eof,
        Res = ok(T0)
    ;
        CharResult = error(Error),
        Res = error(T0, Error)
    ).

io.input_stream_foldl2_io_maybe_stop(Pred, T0, Res, !IO) :-
    io.input_stream(Stream, !IO),
    io.input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO).

io.input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO) :-
    io.read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, Continue, T0, T1, !IO),
        (
            Continue = no,
            Res = ok(T1)
        ;
            Continue = yes,
            io.input_stream_foldl2_io_maybe_stop(Stream, Pred, T1, Res, !IO)
        )
    ;
        CharResult = eof,
        Res = ok(T0)
    ;
        CharResult = error(Error),
        Res = error(T0, Error)
    ).

%-----------------------------------------------------------------------------%

    % Same as ANSI C's clearerr().
    %
:- pred io.clear_err(stream::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.clear_err(Stream::in, IO0::di, IO::uo),
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
    io.clear_err(_Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    // XXX no error flag to reset as in .NET an error is thrown
    // directly as an exception (we should create an error indicator
    // in MF_Mercury_file for compatibility)
}").

:- pragma foreign_proc("Java",
    io.clear_err(_Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX as for .NET above
").

:- pred io.check_err(stream::in, io.res::out, io::di, io::uo) is det.

io.check_err(Stream, Res, !IO) :-
    io.ferror(Stream, Int, Msg, !IO),
    ( Int = 0 ->
        Res = ok
    ;
        Res = error(io_error(Msg))
    ).

    % Similar to ANSI C's ferror().
    %
:- pred io.ferror(stream::in, int::out, string::out, io::di, io::uo) is det.

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

:- pragma foreign_proc("Java",
    ferror(_Stream::in, RetVal::out, _RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    // XXX see clearerr
    RetVal = 0;
}").

:- pred io.make_err_msg(string::in, string::out, io::di, io::uo) is det.

io.make_err_msg(Msg0, Msg, !IO) :-
    io.get_system_error(Error, !IO),
    io.make_err_msg(Error, Msg0, Msg, !IO).

:- pred io.get_system_error(io.system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.get_system_error(Error::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    Error = errno;
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.get_system_error(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    Error = MR_io_exception;
}").

:- pragma foreign_proc("Java",
    io.get_system_error(Error::out, _IO0::di, _IO::uo),
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

:- pragma foreign_proc("Java",
    make_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"{
    if (Error.getMessage() != null) {
        Msg = Msg0 + Error.getMessage();
    } else {
        Msg = Msg0;
    }
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
"
    SUCCESS_INDICATOR = true;
").

:- pragma export(make_win32_err_msg(in, in, out, di, uo),
        "ML_make_win32_err_msg").

make_win32_err_msg(_, _, "", !IO) :-
    ( semidet_succeed ->
        error("io.make_win32_err_msg called for non Win32 back-end")
    ;
        true
    ).

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

    % io.stream_file_size(Stream, Size):
    % If Stream is a regular file, then Size is its size (in bytes),
    % otherwise Size is -1.
    %
:- pred io.stream_file_size(stream::in, int::out, io::di, io::uo) is det.

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_UNISTD_H
    #include <unistd.h>
#endif
#ifdef MR_HAVE_SYS_STAT_H
    #include <sys/stat.h>
#endif
#include ""mercury_types.h""            /* for MR_Integer */
#include ""mercury_library_types.h""    /* for MercuryFilePtr */
").

:- pragma foreign_proc("C",
    io.stream_file_size(Stream::in, Size::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#if defined(MR_HAVE_FSTAT) && \
        (defined(MR_HAVE_FILENO) || defined(fileno)) && defined(S_ISREG)
    struct stat s;
    if (MR_IS_FILE_STREAM(*Stream)) {
        if (fstat(fileno(MR_file(*Stream)), &s) == 0 && S_ISREG(s.st_mode)) {
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
    io.stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (Stream.stream.CanSeek) {
        Size = (int) Stream.stream.Length;
    } else {
        Size = -1;
    }
}").

:- pragma foreign_proc("Java",
    io.stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Size = Stream.size();
").

io.file_modification_time(File, Result, !IO) :-
    io.file_modification_time_2(File, Status, Msg, Time, !IO),
    ( Status = 1 ->
        Result = ok(Time)
    ;
        Result = error(io_error(Msg))
    ).

:- pred io.file_modification_time_2(string::in, int::out, string::out,
    time_t::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.file_modification_time_2(FileName::in, Status::out, Msg::out,
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
        ""io.file_modification_time not available on this platform"");
#endif
    MR_update_io(IO0, IO);

}").
:- pragma foreign_proc("C#",
    io.file_modification_time_2(FileName::in, Status::out, Msg::out,
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

:- pragma foreign_proc("Java",
    io.file_modification_time_2(FileName::in, Status::out, Msg::out,
        Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    java.util.Date date = new java.util.Date();
    try {
        long time = (new java.io.File(FileName)).lastModified();
        if (time == 0) {
            throw new java.lang.Exception(""File not found or I/O error"");
        }
        date.setTime(time);
        Msg = """";
        Status = 1;
    } catch (java.lang.Exception e) {
        Msg = ""lastModified() failed: "" + e.getMessage();
        Status = 0;
    }
    Time = date;
").

%-----------------------------------------------------------------------------%

io.file_type(FollowSymLinks, FileName, MaybeType, !IO) :-
    ( file_type_implemented ->
        FollowSymLinksInt = (FollowSymLinks = yes -> 1 ; 0),
        io.file_type_2(FollowSymLinksInt, FileName, MaybeType, !IO)
    ;
        MaybeType = error(io.make_io_error(
            "Sorry, io.file_type not implemented on this platform"))
    ).

:- pred file_type_implemented is semidet.

file_type_implemented :- semidet_fail.

:- pragma foreign_proc("C",
    file_type_implemented,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HAVE_STAT
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
:- pragma foreign_proc("C#",
    file_type_implemented,
    [will_not_call_mercury, promise_pure, thread_safe],
    "SUCCESS_INDICATOR = true;"
).
:- pragma foreign_proc("Java",
    file_type_implemented,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    succeeded = true;
").

:- pred io.file_type_2(int::in, string::in, io.res(io.file_type)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.file_type_2(FollowSymLinks::in, FileName::in,
        Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
#ifdef MR_HAVE_STAT
    struct stat s;
    int         stat_result;

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
            MR_make_string_const(""io.file_type failed: ""), &Result);
    }
#else
    MR_fatal_error(
        ""Sorry, io.file_type not implemented on this platform"") }
#endif
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.file_type_2(_FollowSymLinks::in, FileName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
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

:- pragma foreign_proc("Java",
    io.file_type_2(_FollowSymLinks::in, FileName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    java.io.File file = new java.io.File(FileName);

    // The Java implementation can distinguish between regular files and
    // directories, and for everything else it just returns unknown.

    if (file.isFile()) {
        Result = new mercury.io.Res_1.Ok_1(new mercury.io.File_type_0(
            mercury.io.File_type_0.regular_file));
    } else if (file.isDirectory()) {
        Result = new mercury.io.Res_1.Ok_1(new mercury.io.File_type_0(
            mercury.io.File_type_0.directory));
    } else {
        Result = new mercury.io.Res_1.Ok_1(new mercury.io.File_type_0(
            mercury.io.File_type_0.unknown));
    }
").

:- func file_type_character_device = file_type.
:- func file_type_block_device = file_type.
:- func file_type_fifo = file_type.
:- func file_type_directory = file_type.
:- func file_type_socket = file_type.
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

io.check_file_accessibility(FileName, AccessTypes, Result, !IO) :-
    ( have_dotnet ->
        io.check_file_accessibility_dotnet(FileName, AccessTypes, Result, !IO)
    ;
        io.check_file_accessibility_2(FileName, AccessTypes, Result, !IO)
    ).

:- pred io.check_file_accessibility_2(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.check_file_accessibility_2(FileName::in, AccessTypes::in,
        Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
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
            MR_make_string_const(""file not accessible: ""), &Result);
    }
#else /* !MR_HAVE_ACCESS */
    Result = ML_make_io_res_0_error_msg(
        ""io.check_file_accessibility not supported on this platform"");
#endif
    IO = IO0;
}").

:- pragma foreign_proc("Java",
    io.check_file_accessibility_2(FileName::in, AccessTypes::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    java.lang.String permissions = null;

    if (access_types_includes_read_1_p_0((mercury.list.List_1) AccessTypes)) {
        permissions = ""read"";
    }

    if (access_types_includes_write_1_p_0((mercury.list.List_1) AccessTypes)) {
        if (permissions == null) {
            permissions = ""write"";
        } else {
            permissions = ""read,write"";
        }
    }

    if (access_types_includes_execute_1_p_0((mercury.list.List_1) AccessTypes))
    {
        if (permissions == null) {
            permissions = ""execute"";
        } else {
            permissions = permissions + "",execute"";
        }
    }

    try {
        if (permissions != null) {
            java.lang.System.getSecurityManager().checkPermission(
                new java.io.FilePermission(FileName, permissions));
        }
        Result = make_io_res_0_ok_0_f_0();
    }
    catch (java.lang.Exception e) {
        Result = make_io_res_0_error_msg_1_f_0(e.getMessage());
    }
").

:- pred io.check_file_accessibility_dotnet(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.

io.check_file_accessibility_dotnet(FileName, AccessTypes, Result, !IO) :-
    % The .NET CLI doesn't provide an equivalent of access(), so we have to
    % try to open the file to see if it is accessible.

    CheckRead0 = pred_to_bool(access_types_includes_read(AccessTypes)),
    CheckWrite = pred_to_bool(access_types_includes_write(AccessTypes)),

    CheckExec = pred_to_bool(access_types_includes_execute(AccessTypes)),
    % We need to be able to read a file to execute it.
    CheckRead = bool.or(CheckRead0, CheckExec),

    io.file_type(yes, FileName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(FileType),
        ( FileType = directory ->
            check_directory_accessibility_dotnet(FileName,
                to_int(CheckRead), to_int(CheckWrite), Result, !IO)
        ;
            ( CheckRead = yes ->
                io.open_input(FileName, InputRes, !IO),
                (
                    InputRes = ok(InputStream),
                    io.close_input(InputStream, !IO),
                    CheckReadRes = ok
                ;
                    InputRes = error(InputError),
                    CheckReadRes = error(InputError)
                )
            ;
                CheckReadRes = ok
            ),
            (
                CheckReadRes = ok,
                CheckWrite = yes
            ->
                io.open_append(FileName, OutputRes, !IO),
                (
                    OutputRes = ok(OutputStream),
                    io.close_output(OutputStream, !IO),
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
                % Unix programs need to check whether the execute bit is set
                % for the directory, but we can't actually execute the
                % directory.
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

:- pred have_dotnet_exec_permission(io.res::out, io::di, io::uo) is det.

have_dotnet_exec_permission(Res, !IO) :-
    % Avoid determinism warnings.
    ( semidet_succeed ->
        error("io.have_dotnet_exec_permission invoked " ++
            "for non-.NET CLI backend")
    ;
        % Never reached.
        Res = ok
    ).

:- pragma foreign_proc("C#",
    have_dotnet_exec_permission(Result::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe, terminates],
"{
    try {
        // We need unrestricted permissions to execute
        // unmanaged code.
        (new System.Security.Permissions.SecurityPermission(
            System.Security.Permissions.SecurityPermissionFlag.
            AllFlags)).Demand();
        Result = mercury.io.mercury_code.ML_make_io_res_0_ok();
    } catch (System.Exception e) {
        mercury.io.mercury_code.ML_make_io_res_0_error(e,
            ""execute permission check failed: "", ref Result);
    }

}").

:- pred check_directory_accessibility_dotnet(string::in, int::in, int::in,
    io.res::out, io::di, io::uo) is det.

check_directory_accessibility_dotnet(_, _, _, Res, !IO) :-
    % Avoid determinism warnings.
    ( semidet_succeed ->
        error("io.check_directory_accessibility_dotnet called " ++
            "for non-.NET CLI backend")
    ;
        % Never reached.
        Res = ok
    ).

:- pragma foreign_proc("C#",
    check_directory_accessibility_dotnet(FileName::in, CheckRead::in,
        CheckWrite::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io, thread_safe, terminates],
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

            // XXX This isn't quite right. Just because the directory
            // isn't read-only doesn't mean we have permission to write to it.
            // The only way to test whether a directory is writable is to
            // write a file to it. The ideal way to do that would be
            // io.make_temp, but currently the .NET backend version of that
            // ignores the directory passed to it.
            System.IO.FileAttributes attrs =
                System.IO.File.GetAttributes(FileName);
            if ((attrs & System.IO.FileAttributes.ReadOnly) ==
                System.IO.FileAttributes.ReadOnly)
            {
                throw (new System.Exception(""file is read-only""));
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

access_types_includes_read(Access) :-
    list.member(read, Access).

:- pred access_types_includes_write(list(access_type)::in) is semidet.
:- pragma export(access_types_includes_write(in),
    "ML_access_types_includes_write").

access_types_includes_write(Access) :-
    list.member(write, Access).

:- pred access_types_includes_execute(list(access_type)::in) is semidet.
:- pragma export(access_types_includes_execute(in),
    "ML_access_types_includes_execute").

access_types_includes_execute(Access) :-
    list.member(execute, Access).

:- func make_io_res_0_ok = io.res.
:- pragma export((make_io_res_0_ok = out), "ML_make_io_res_0_ok").

make_io_res_0_ok = ok.

:- pred make_io_res_0_error(io.system_error::in, string::in, io.res::out,
    io::di, io::uo) is det.
:- pragma export(make_io_res_0_error(in, in, out, di, uo),
    "ML_make_io_res_0_error").

make_io_res_0_error(Error, Msg0, error(make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, Msg0, Msg, !IO).

:- func make_io_res_0_error_msg(string) = io.res.
:- pragma export((make_io_res_0_error_msg(in) = out),
    "ML_make_io_res_0_error_msg").

make_io_res_0_error_msg(Msg) = error(make_io_error(Msg)).

:- func make_io_res_1_ok_file_type(file_type) = io.res(file_type).
:- pragma export((make_io_res_1_ok_file_type(in) = out),
    "ML_make_io_res_1_ok_file_type").

make_io_res_1_ok_file_type(FileType) = ok(FileType).

:- pred make_io_res_1_error_file_type(io.system_error::in,
    string::in, io.res(file_type)::out, io::di, io::uo) is det.
:- pragma export(make_io_res_1_error_file_type(in, in, out, di, uo),
    "ML_make_io_res_1_error_file_type").

make_io_res_1_error_file_type(Error, Msg0, error(make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, Msg0, Msg, !IO).

%-----------------------------------------------------------------------------%

:- type file_id ---> file_id.
:- pragma foreign_type("C", file_id, "ML_File_Id")
    where comparison is compare_file_id.

:- pragma foreign_decl("C",
"
#ifdef MR_HAVE_DEV_T
  typedef   dev_t       ML_dev_t;
#else
  typedef   MR_Integer  ML_dev_t;
#endif

#ifdef MR_HAVE_INO_T
  typedef   ino_t       ML_ino_t;
#else
  typedef   MR_Integer  ML_ino_t;
#endif

typedef struct {
    ML_dev_t device;
    ML_ino_t inode;
} ML_File_Id;
").

:- pred compare_file_id(comparison_result::uo, file_id::in, file_id::in)
    is det.

compare_file_id(Result, FileId1, FileId2) :-
    compare_file_id_2(Result0, FileId1, FileId2),
    ( Result0 < 0 ->
        Result = (<)
    ; Result0 = 0 ->
        Result = (=)
    ;
        Result = (>)
    ).

:- pred compare_file_id_2(int::out, file_id::in, file_id::in) is det.

:- pragma foreign_proc("C",
    compare_file_id_2(Res::out, FileId1::in, FileId2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int device_cmp;
    int inode_cmp;

    /*
    ** For compilers other than GCC, glibc defines dev_t as struct (dev_t
    ** is 64 bits, and other compilers may not have a 64 bit arithmetic type).
    ** XXX This code assumes that dev_t and ino_t do not include padding bits.
    ** In practice, that should be OK.
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

:- pragma foreign_proc("Java",
    compare_file_id_2(_Res::out, _FileId1::in, _FileId2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    throw new java.lang.RuntimeException(
        ""File IDs are not supported by Java."");
").

io.file_id(FileName, Result, !IO) :-
    ( have_file_ids ->
        io.file_id_2(FileName, Status, Msg, FileId, !IO),
        ( Status = 1 ->
            Result = ok(FileId)
        ;
            Result = error(io_error(Msg))
        )
    ;
        Result = error(make_io_error("io.file_id not implemented " ++
            "on this platform"))
    ).

:- pred io.file_id_2(string::in, int::out, string::out, file_id::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.file_id_2(FileName::in, Status::out, Msg::out,
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

:- pragma foreign_proc("Java",
    io.file_id_2(_FileName::in, _Status::out, _Msg::out,
        _FileId::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    // This function should never be called, since have_file_ids will
    // fail for Java.
    if (true) { // otherwise Java complains about unreachable stmts.
        throw new RuntimeException(
            ""io.file_id_2 called but not supported"");
    }
").

% Can we retrieve inode numbers on this system.
have_file_ids :- semidet_fail.
:- pragma foreign_proc("C",
    have_file_ids,
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
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
:- pragma foreign_type(c, buffer, "MR_Char *", [can_pass_as_mercury_type]).

    % XXX It would be better to use a char_array (e.g. defined as char[] in
    % C#) type rather than array(char).  This is because on the Java and IL
    % backends indexing into an array whose element type is known
    % statically requires less overhead.
:- type buffer ---> buffer(array(char)).

    % XXX Extend the workaround for no `ui' modes in array.m.
:- inst uniq_buffer == bound(buffer(uniq_array)).
:- mode buffer_di == di(uniq_buffer).
:- mode buffer_uo == out(uniq_buffer).

:- pred io.alloc_buffer(int::in, buffer::buffer_uo) is det.

:- pragma foreign_proc("C",
    io.alloc_buffer(Size::in, Buffer::buffer_uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    MR_Word buf;
    MR_offset_incr_hp_atomic_msg(buf, 0,
        (Size * sizeof(MR_Char) + sizeof(MR_Word) - 1) / sizeof(MR_Word),
        MR_PROC_LABEL, ""io:buffer/0"");
    Buffer = (MR_Char *) buf;
}").

io.alloc_buffer(Size, buffer(Array)) :-
    % XXX '0' is used as Mercury doesn't recognise '\0' as a char constant.
    array.init(Size, '0', Array).

:- pred io.resize_buffer(int::in, int::in,
    buffer::buffer_di, buffer::buffer_uo) is det.

:- pragma foreign_proc("C",
    io.resize_buffer(OldSize::in, NewSize::in,
        Buffer0::buffer_di, Buffer::buffer_uo),
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
            MR_memcpy(Buffer, Buffer0, NewSize);
        } else {
            MR_memcpy(Buffer, Buffer0, OldSize);
        }
    }
#endif
}").

io.resize_buffer(_OldSize, NewSize, buffer(Array0), buffer(Array)) :-
    % XXX '0' is used as Mercury doesn't recognise '\0' as a char constant.
    array.resize(Array0, NewSize, '0', Array).

:- pred io.buffer_to_string(buffer::buffer_di, int::in, string::uo) is det.

:- pragma foreign_proc("C",
    io.buffer_to_string(Buffer::buffer_di, Len::in, Str::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    Str = Buffer;
    Str[Len] = '\\0';
}").

io.buffer_to_string(buffer(Array), Len, from_char_list(List)) :-
    array.fetch_items(Array, min(Array), min(Array) + Len - 1, List).

:- pred io.read_into_buffer(stream::in, buffer::buffer_di, buffer::buffer_uo,
    int::in, int::out, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.read_into_buffer(Stream::in, Buffer0::buffer_di, Buffer::buffer_uo,
        Pos0::in, Pos::out, Size::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    int items_read;

    MR_CHECK_EXPR_TYPE(Buffer0, MR_Char *);
    MR_CHECK_EXPR_TYPE(Buffer, MR_Char *);

    items_read = MR_READ(*Stream, Buffer0 + Pos0, Size - Pos0);

    Buffer = Buffer0;
    Pos = Pos0 + items_read;
    MR_update_io(IO0, IO);
}").

io.read_into_buffer(Stream, buffer(Array0), buffer(Array), !Pos, Size, !IO) :-
    io.read_into_array(Stream, Array0, Array, !Pos, Size, !IO).

:- pred io.read_into_array(stream::in,
    array(char)::array_di, array(char)::array_uo, int::in, int::out,
    int::in, io::di, io::uo) is det.

io.read_into_array(Stream, !Array, !Pos, Size, !IO) :-
    ( !.Pos >= Size ->
        true
    ;
        io.read_char(Stream, CharResult, !IO),
        ( CharResult = ok(Char) ->
            array.set(!.Array, !.Pos, Char, !:Array),
            !:Pos = !.Pos + 1,
            io.read_into_array(Stream, !Array, !Pos, Size, !IO)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

io.read_binary_file(Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_binary_file(Stream, Result, !IO).

io.read_binary_file(Stream, Result, !IO) :-
    io.read_binary_file_2(Stream, [], Result, !IO).

:- pred io.read_binary_file_2(io.input_stream::in, list(int)::in,
    io.result(list(int))::out, io::di, io::uo) is det.

io.read_binary_file_2(Stream, Bytes0, Result, !IO) :-
    io.read_byte(Stream, Result0, !IO),
    (
        Result0 = eof,
        list.reverse(Bytes0, Bytes),
        Result = ok(Bytes)
    ;
        Result0 = error(Err),
        Result = error(Err)
    ;
        Result0 = ok(Byte),
        io.read_binary_file_2(Stream, [Byte | Bytes0], Result, !IO)
    ).

%-----------------------------------------------------------------------------%

io.binary_input_stream_foldl(Pred, T0, Res, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.binary_input_stream_foldl(Stream, Pred, T0, Res, !IO).

io.binary_input_stream_foldl(Stream, Pred, T0, Res, !IO) :-
    io.read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, T0, T1),
        io.binary_input_stream_foldl(Stream, Pred, T1, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok(T0)
    ;
        ByteResult = error(Error),
        Res = error(T0, Error)
    ).

io.binary_input_stream_foldl_io(Pred, Res, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.binary_input_stream_foldl_io(Stream, Pred, Res, !IO).

io.binary_input_stream_foldl_io(Stream, Pred, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        io.binary_input_stream_foldl_io_plain(Stream, Pred, Res, !IO)
    ;
        ShouldReduce = yes,
        io.binary_input_stream_foldl_io_chunk(Stream, Pred, Res, !IO)
    ).

:- pred io.binary_input_stream_foldl_io_plain(io.binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode io.binary_input_stream_foldl_io_plain(in,
    (pred(in, di, uo) is det), out, di, uo) is det.
:- mode io.binary_input_stream_foldl_io_plain(in,
    (pred(in, di, uo) is cc_multi), out, di, uo) is cc_multi.

io.binary_input_stream_foldl_io_plain(Stream, Pred, Res, !IO) :-
    io.read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, !IO),
        io.binary_input_stream_foldl_io_plain(Stream, Pred, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok
    ;
        ByteResult = error(Error),
        Res = error(Error)
    ).

:- pred io.binary_input_stream_foldl_io_chunk(io.binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode io.binary_input_stream_foldl_io_chunk(in,
    (pred(in, di, uo) is det), out, di, uo) is det.
:- mode io.binary_input_stream_foldl_io_chunk(in,
    (pred(in, di, uo) is cc_multi), out, di, uo) is cc_multi.

io.binary_input_stream_foldl_io_chunk(Stream, Pred, Res, !IO) :-
    io.binary_input_stream_foldl_io_inner(chunk_size, Stream, Pred,
        InnerRes, !IO),
    (
        InnerRes = ok,
        Res = ok
    ;
        InnerRes = error(Error),
        Res = error(Error)
    ;
        InnerRes = more,
        io.binary_input_stream_foldl_io_chunk(Stream, Pred, Res, !IO)
    ).

:- pred io.binary_input_stream_foldl_io_inner(int, io.binary_input_stream,
    pred(int, io, io), chunk_inner_res0, io, io).
:- mode io.binary_input_stream_foldl_io_inner(in, in,
    (pred(in, di, uo) is det), out, di, uo) is det.
:- mode io.binary_input_stream_foldl_io_inner(in, in,
    (pred(in, di, uo) is cc_multi), out, di, uo) is cc_multi.

io.binary_input_stream_foldl_io_inner(Left, Stream, Pred, Res, !IO) :-
    ( Left > 0 ->
        io.read_byte(Stream, ByteResult, !IO),
        (
            ByteResult = ok(Byte),
            Pred(Byte, !IO),
            io.binary_input_stream_foldl_io_inner(Left - 1,
                Stream, Pred, Res, !IO)
        ;
            ByteResult = eof,
            Res = ok
        ;
            ByteResult = error(Error),
            Res = error(Error)
        )
    ;
        Res = more
    ).

io.binary_input_stream_foldl2_io(Pred, T0, Res, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.binary_input_stream_foldl2_io(Stream, Pred, T0, Res, !IO).

io.binary_input_stream_foldl2_io(Stream, Pred, T0, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        io.binary_input_stream_foldl2_io_plain(Stream, Pred, T0, Res, !IO)
    ;
        ShouldReduce = yes,
        io.binary_input_stream_foldl2_io_chunk(Stream, Pred, T0, Res, !IO)
    ).

:- pred io.binary_input_stream_foldl2_io_plain(io.binary_input_stream,
    pred(int, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_plain(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_plain(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

io.binary_input_stream_foldl2_io_plain(Stream, Pred, T0, Res, !IO) :-
    io.read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, T0, T1, !IO),
        io.binary_input_stream_foldl2_io_plain(Stream, Pred, T1, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok(T0)
    ;
        ByteResult = error(Error),
        Res = error(T0, Error)
    ).

:- pred io.binary_input_stream_foldl2_io_chunk(io.binary_input_stream,
    pred(int, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_chunk(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_chunk(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

io.binary_input_stream_foldl2_io_chunk(Stream, Pred, T0, Res, !IO) :-
    io.binary_input_stream_foldl2_io_inner(chunk_size, Stream, Pred, T0,
        InnerRes, !IO),
    (
        InnerRes = ok(T),
        Res = ok(T)
    ;
        InnerRes = error(T, Error),
        Res = error(T, Error)
    ;
        InnerRes = more(T1),
        io.binary_input_stream_foldl2_io_chunk(Stream, Pred, T1, Res, !IO)
    ).

:- pred io.binary_input_stream_foldl2_io_inner(int, io.binary_input_stream,
    pred(int, T, T, io, io), T, chunk_inner_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_inner(in, in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_inner(in, in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

io.binary_input_stream_foldl2_io_inner(Left, Stream, Pred, T0, Res, !IO) :-
    ( Left > 0 ->
        io.read_byte(Stream, ByteResult, !IO),
        (
            ByteResult = ok(Byte),
            Pred(Byte, T0, T1, !IO),
            io.binary_input_stream_foldl2_io_inner(Left - 1,
                Stream, Pred, T1, Res, !IO)
        ;
            ByteResult = eof,
            Res = ok(T0)
        ;
            ByteResult = error(Error),
            Res = error(T0, Error)
        )
    ;
        Res = more(T0)
    ).

io.binary_input_stream_foldl2_io_maybe_stop(Pred, T0, Res, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.binary_input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO).

io.binary_input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        io.binary_input_stream_foldl2_io_maybe_stop_plain(Stream,
            Pred, T0, Res, !IO)
    ;
        ShouldReduce = yes,
        io.binary_input_stream_foldl2_io_maybe_stop_chunk(Stream,
            Pred, T0, Res, !IO)
    ).

:- pred io.binary_input_stream_foldl2_io_maybe_stop_plain(
    io.binary_input_stream, pred(int, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_maybe_stop_plain(
    in, (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_maybe_stop_plain(
    in, (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

io.binary_input_stream_foldl2_io_maybe_stop_plain(Stream, Pred, T0, Res,
        !IO) :-
    io.read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, Continue, T0, T1, !IO),
        (
            Continue = no,
            Res = ok(T1)
        ;
            Continue = yes,
            io.binary_input_stream_foldl2_io_maybe_stop_plain(
                Stream, Pred, T1, Res, !IO)
        )
    ;
        ByteResult = eof,
        Res = ok(T0)
    ;
        ByteResult = error(Error),
        Res = error(T0, Error)
    ).

:- pred io.binary_input_stream_foldl2_io_maybe_stop_chunk(
    io.binary_input_stream, pred(int, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_maybe_stop_chunk(
    in, (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_maybe_stop_chunk(
    in, (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

io.binary_input_stream_foldl2_io_maybe_stop_chunk(Stream, Pred, T0, Res,
        !IO) :-
    io.binary_input_stream_foldl2_io_maybe_stop_inner(chunk_size,
        Stream, Pred, T0, InnerRes, !IO),
    (
        InnerRes = ok(T),
        Res = ok(T)
    ;
        InnerRes = error(T, Error),
        Res = error(T, Error)
    ;
        InnerRes = more(T1),
        io.binary_input_stream_foldl2_io_maybe_stop_chunk(Stream,
            Pred, T1, Res, !IO)
    ).

:- pred io.binary_input_stream_foldl2_io_maybe_stop_inner(int,
    io.binary_input_stream, pred(int, bool, T, T, io, io),
    T, chunk_inner_res(T), io, io).
:- mode io.binary_input_stream_foldl2_io_maybe_stop_inner(in,
    in, (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.binary_input_stream_foldl2_io_maybe_stop_inner(in,
    in, (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_maybe_stop_inner(Left, Stream, Pred, T0, Res,
        !IO) :-
    ( Left > 0 ->
        io.read_byte(Stream, ByteResult, !IO),
        (
            ByteResult = ok(Byte),
            Pred(Byte, Continue, T0, T1, !IO),
            (
                Continue = no,
                Res = ok(T1)
            ;
                Continue = yes,
                binary_input_stream_foldl2_io_maybe_stop_inner(
                    Left - 1, Stream, Pred, T1, Res, !IO)
            )
        ;
            ByteResult = eof,
            Res = ok(T0)
        ;
            ByteResult = error(Error),
            Res = error(T0, Error)
        )
    ;
        Res = more(T0)
    ).

:- type chunk_inner_res0
    --->    ok
    ;       error(io.error)
    ;       more.

:- type chunk_inner_res(T)
    --->    ok(T)
    ;       error(T, io.error)
    ;       more(T).

:- pred should_reduce_stack_usage(bool::out) is det.

% For non-C backends.
should_reduce_stack_usage(yes).

:- pragma foreign_proc("C",
    should_reduce_stack_usage(ShouldReduce::out),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_EXEC_TRACE
    ShouldReduce = MR_TRUE;
#else
    ShouldReduce = MR_FALSE;
#endif
").

    % Chunk_size gives the maximum number of recursive calls we want to
    % allow in the binary_input_stream_foldl*_inner predicates. Without
    % such a limit, the depth of recursion, which depends on the size of
    % the file they read, will cause exhaustion of the det stack in debug
    % grades, since there is no tail recursion in such grades.
    %
    % With this arrangement, the maximum number of stack frames needed
    % to process a file of size N is N/1000 + 1000, the former being the
    % number of frames of binary_input_stream_foldl*_chunk predicates,
    % the latter being the max number of frames of the *_inner predicates.
    %
:- func chunk_size = int.

chunk_size = 1000.

%-----------------------------------------------------------------------------%

io.putback_char(Char, !IO) :-
    io.input_stream(Stream, !IO),
    io.putback_char(Stream, Char, !IO).

io.putback_byte(Char, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.putback_byte(Stream, Char, !IO).

io.read(Result, !IO) :-
    term_io.read_term(ReadResult, !IO),
    io.get_line_number(LineNumber, !IO),
    io.process_read_term(ReadResult, LineNumber, Result).

io.read_from_string(FileName, String, Len, Result, !Posn) :-
    parser.read_term_from_string(FileName, String, Len, !Posn, ReadResult),
    !.Posn = posn(LineNumber, _, _),
    io.process_read_term(ReadResult, LineNumber, Result).

:- pred io.process_read_term(read_term::in, int::in, io.read_result(T)::out)
    is det.

io.process_read_term(ReadResult, LineNumber, Result) :-
    (
        ReadResult = term(_VarSet, Term),
        (
            term_to_type(Term, Type)
        ->
            Result = ok(Type)
        ;
            ( term.is_ground(Term) ->
                Result = error(
                    "io.read: the term read did not have the right type",
                    LineNumber)
            ;
                Result = error("io.read: the term read was not a ground term",
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

io.read(Stream, Result, !IO) :-
    io.set_input_stream(Stream, OrigStream, !IO),
    io.read(Result, !IO),
    io.set_input_stream(OrigStream, _Stream, !IO).

io.ignore_whitespace(Result, !IO) :-
    io.input_stream(Stream, !IO),
    io.ignore_whitespace(Stream, Result, !IO).

io.ignore_whitespace(Stream, Result, !IO) :-
    io.read_char(Stream, CharResult, !IO),
    (
        CharResult = error(Error),
        Result = error(Error)
    ;
        CharResult = eof,
        Result = eof
    ;
        CharResult = ok(Char),
        ( char.is_whitespace(Char) ->
            io.ignore_whitespace(Stream, Result, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            Result = ok
        )
    ).

%-----------------------------------------------------------------------------%

% Output predicates.

io.nl(!IO) :-
    io.write_char('\n', !IO).

io.nl(Stream, !IO) :-
    io.write_char(Stream, '\n', !IO).

io.write_strings(Strings, !IO) :-
    io.output_stream(Stream, !IO),
    io.write_strings(Stream, Strings, !IO).

io.write_strings(_Stream, [], !IO).
io.write_strings(Stream, [S | Ss], !IO) :-
    io.write_string(Stream, S, !IO),
    io.write_strings(Stream, Ss, !IO).

io.format(FormatString, Arguments, !IO) :-
    io.output_stream(Stream, !IO),
    io.format(Stream, FormatString, Arguments, !IO).

io.format(Stream, FormatString, Arguments, !IO) :-
    string.format(FormatString, Arguments, String),
    io.write_string(Stream, String, !IO).

io.write_many(Poly_list, !IO) :-
    io.output_stream(Stream, !IO),
    io.write_many(Stream, Poly_list, !IO).

io.write_many(_Stream, [], !IO).
io.write_many(Stream, [c(C) | Rest], !IO) :-
    io.write_char(Stream, C, !IO),
    io.write_many(Stream, Rest, !IO).
io.write_many(Stream, [i(I) | Rest], !IO) :-
    io.write_int(Stream, I, !IO),
    io.write_many(Stream, Rest, !IO).
io.write_many(Stream, [s(S) | Rest], !IO) :-
    io.write_string(Stream, S, !IO),
    io.write_many(Stream, Rest, !IO).
io.write_many(Stream, [f(F) | Rest], !IO) :-
    io.write_float(Stream, F, !IO),
    io.write_many(Stream, Rest, !IO).

%-----------------------------------------------------------------------------%

% Various different versions of io.print.

:- pragma export(io.print(in, in(do_not_allow), in, di, uo),
    "ML_io_print_dna_to_stream").
:- pragma export(io.print(in, in(canonicalize), in, di, uo),
    "ML_io_print_can_to_stream").
:- pragma export(io.print(in, in(include_details_cc), in, di, uo),
    "ML_io_print_cc_to_stream").

io.print(Stream, NonCanon, Term, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.do_print(NonCanon, Term, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

:- pragma export(io.print(in, in, di, uo), "ML_io_print_to_stream").

io.print(Stream, Term, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.do_print(canonicalize, Term, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

:- pragma export(io.print(in, di, uo), "ML_io_print_to_cur_stream").

io.print(Term, !IO) :-
    io.do_print(canonicalize, Term, !IO).

io.print_cc(Term, !IO) :-
    io.do_print(include_details_cc, Term, !IO).

:- pred io.do_print(deconstruct.noncanon_handling, T, io, io).
:- mode io.do_print(in(do_not_allow), in, di, uo) is det.
:- mode io.do_print(in(canonicalize), in, di, uo) is det.
:- mode io.do_print(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.do_print(in, in, di, uo) is cc_multi.

io.do_print(NonCanon, Term, !IO) :-
    % `string', `char' and `univ' are special cases for io.print
    type_to_univ(Term, Univ),
    ( univ_to_type(Univ, String) ->
        io.write_string(String, !IO)
    ; univ_to_type(Univ, Char) ->
        io.write_char(Char, !IO)
    ; univ_to_type(Univ, OrigUniv) ->
        io.write_univ(OrigUniv, !IO)
    ;
        io.print_quoted(NonCanon, Term, !IO)
    ).

:- pred io.print_quoted(deconstruct.noncanon_handling, T, io, io).
:- mode io.print_quoted(in(do_not_allow), in, di, uo) is det.
:- mode io.print_quoted(in(canonicalize), in, di, uo) is det.
:- mode io.print_quoted(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.print_quoted(in, in, di, uo) is cc_multi.

io.print_quoted(NonCanon, Term, !IO) :-
    io.do_write(NonCanon, Term, !IO).
% When we have runtime type classes membership tests, then instead
% of io.write(Term), we will want to do something like
%   ( univ_to_type_class(Univ, Portrayable) ->
%       portray(Portrayable, !IO)
%   ;
%       ... code like io.write, but which prints the arguments
%       using io.print_quoted, rather than io.write ...
%   )

%-----------------------------------------------------------------------------%

% Various different versions of io.write.

io.write(Stream, NonCanon, X, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.do_write(NonCanon, X, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

io.write(Stream, X, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.do_write(canonicalize, X, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

io.write(X, !IO) :-
    io.do_write(canonicalize, X, !IO).

io.write_cc(X, !IO) :-
    io.do_write(include_details_cc, X, !IO).

:- pred io.do_write(deconstruct.noncanon_handling, T, io, io).
:- mode io.do_write(in(do_not_allow), in, di, uo) is det.
:- mode io.do_write(in(canonicalize), in, di, uo) is det.
:- mode io.do_write(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.do_write(in, in, di, uo) is cc_multi.

io.do_write(NonCanon, Term, !IO) :-
    type_to_univ(Term, Univ),
    io.do_write_univ(NonCanon, Univ, !IO).

%-----------------------------------------------------------------------------%

% Various different versions of io.write_univ.

io.write_univ(Univ, !IO) :-
    io.do_write_univ(canonicalize, Univ, !IO).

io.write_univ(Stream, Univ, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.do_write_univ(canonicalize, Univ, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

io.write_univ(Stream, NonCanon, Univ, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.do_write_univ(NonCanon, Univ, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

:- pred io.do_write_univ(deconstruct.noncanon_handling, univ, io, io).
:- mode io.do_write_univ(in(do_not_allow), in, di, uo) is det.
:- mode io.do_write_univ(in(canonicalize), in, di, uo) is det.
:- mode io.do_write_univ(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.do_write_univ(in, in, di, uo) is cc_multi.

io.do_write_univ(NonCanon, Univ, !IO) :-
    io.get_op_table(OpTable, !IO),
    io.do_write_univ(NonCanon, Univ, ops.max_priority(OpTable) + 1, !IO).

:- pred io.do_write_univ(deconstruct.noncanon_handling, univ, ops.priority,
    io, io).
:- mode io.do_write_univ(in(do_not_allow), in, in, di, uo) is det.
:- mode io.do_write_univ(in(canonicalize), in, in, di, uo) is det.
:- mode io.do_write_univ(in(include_details_cc), in, in, di, uo) is cc_multi.
:- mode io.do_write_univ(in, in, in, di, uo) is cc_multi.

io.do_write_univ(NonCanon, Univ, Priority, !IO) :-
    % We need to special-case the builtin types:
    %   int, char, float, string
    %   type_info, univ, c_pointer, array
    %   and private_builtin.type_info
    %
    ( univ_to_type(Univ, String) ->
        term_io.quote_string(String, !IO)
    ; univ_to_type(Univ, Char) ->
        term_io.quote_char(Char, !IO)
    ; univ_to_type(Univ, Int) ->
        io.write_int(Int, !IO)
    ; univ_to_type(Univ, Float) ->
        io.write_float(Float, !IO)
    ; univ_to_type(Univ, TypeDesc) ->
        io.write_type_desc(TypeDesc, !IO)
    ; univ_to_type(Univ, TypeCtorDesc) ->
        io.write_type_ctor_desc(TypeCtorDesc, !IO)
    ; univ_to_type(Univ, Stream) ->
        io.get_stream_db(StreamDb, !IO),
        io.maybe_stream_info(StreamDb, Stream) = StreamInfo,
        type_to_univ(StreamInfo, StreamInfoUniv),
        io.do_write_univ(NonCanon, StreamInfoUniv, Priority, !IO)
    ; univ_to_type(Univ, C_Pointer) ->
        io.write_c_pointer(C_Pointer, !IO)
    ;
        % Check if the type is array.array/1. We can't just use univ_to_type
        % here since array.array/1 is a polymorphic type.
        %
        % The calls to type_ctor_name and type_ctor_module_name are not really
        % necessary -- we could use univ_to_type in the condition instead
        % of det_univ_to_type in the body. However, this way of doing things
        % is probably more efficient in the common case when the thing being
        % printed is *not* of type array.array/1.
        %
        % The ordering of the tests here (arity, then name, then module name,
        % rather than the reverse) is also chosen for efficiency, to find
        % failure cheaply in the common cases, rather than for readability.
        %
        type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "array",
        type_ctor_module_name(TypeCtor) = "array"
    ->
        % Now that we know the element type, we can constrain the type
        % of the variable `Array' so that we can use det_univ_to_type.

        has_type(Elem, ElemType),
        same_array_elem_type(Array, Elem),
        det_univ_to_type(Univ, Array),
        io.write_array(Array, !IO)
    ;
        % Check if the type is private_builtin.type_info/1.
        % See the comments above for array.array/1.

        type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "type_info",
        type_ctor_module_name(TypeCtor) = "private_builtin"
    ->
        has_type(Elem, ElemType),
        same_private_builtin_type(PrivateBuiltinTypeInfo, Elem),
        det_univ_to_type(Univ, PrivateBuiltinTypeInfo),
        io.write_private_builtin_type_info(PrivateBuiltinTypeInfo, !IO)
    ;
        io.write_ordinary_term(NonCanon, Univ, Priority, !IO)
    ).

:- pred same_array_elem_type(array(T)::unused, T::unused) is det.

same_array_elem_type(_, _).

:- pred same_private_builtin_type(private_builtin.type_info::unused,
    T::unused) is det.

same_private_builtin_type(_, _).

:- pred io.write_ordinary_term(deconstruct.noncanon_handling, univ,
    ops.priority, io, io).
:- mode io.write_ordinary_term(in(do_not_allow), in, in, di, uo) is det.
:- mode io.write_ordinary_term(in(canonicalize), in, in, di, uo) is det.
:- mode io.write_ordinary_term(in(include_details_cc), in, in, di, uo)
    is cc_multi.
:- mode io.write_ordinary_term(in, in, in, di, uo) is cc_multi.

io.write_ordinary_term(NonCanon, Univ, Priority, !IO) :-
    univ_value(Univ) = Term,
    deconstruct.deconstruct(Term, NonCanon, Functor, _Arity, Args),
    io.get_op_table(OpTable, !IO),
    (
        Functor = "[|]",
        Args = [ListHead, ListTail]
    ->
        io.write_char('[', !IO),
        io.write_arg(NonCanon, ListHead, !IO),
        io.write_list_tail(NonCanon, ListTail, !IO),
        io.write_char(']', !IO)
    ;
        Functor = "[]",
        Args = []
    ->
        io.write_string("[]", !IO)
    ;
        Functor = "{}",
        Args = [BracedTerm]
    ->
        io.write_string("{ ", !IO),
        io.do_write_univ(NonCanon, BracedTerm, !IO),
        io.write_string(" }", !IO)
    ;
        Functor = "{}",
        Args = [BracedHead | BracedTail]
    ->
        io.write_char('{', !IO),
        io.write_arg(NonCanon, BracedHead, !IO),
        io.write_term_args(NonCanon, BracedTail, !IO),
        io.write_char('}', !IO)
    ;
        Args = [PrefixArg],
        ops.lookup_prefix_op(OpTable, Functor, OpPriority, OpAssoc)
    ->
        maybe_write_paren('(', Priority, OpPriority, !IO),
        term_io.quote_atom(Functor, !IO),
        io.write_char(' ', !IO),
        adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
        io.do_write_univ(NonCanon, PrefixArg, NewPriority, !IO),
        maybe_write_paren(')', Priority, OpPriority, !IO)
    ;
        Args = [PostfixArg],
        ops.lookup_postfix_op(OpTable, Functor, OpPriority, OpAssoc)
    ->
        maybe_write_paren('(', Priority, OpPriority, !IO),
        adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
        io.do_write_univ(NonCanon, PostfixArg, NewPriority, !IO),
        io.write_char(' ', !IO),
        term_io.quote_atom(Functor, !IO),
        maybe_write_paren(')', Priority, OpPriority, !IO)
    ;
        Args = [Arg1, Arg2],
        ops.lookup_infix_op(OpTable, Functor, OpPriority,
            LeftAssoc, RightAssoc)
    ->
        maybe_write_paren('(', Priority, OpPriority, !IO),
        adjust_priority_for_assoc(OpPriority, LeftAssoc, LeftPriority),
        io.do_write_univ(NonCanon, Arg1, LeftPriority, !IO),
        ( Functor = "," ->
            io.write_string(", ", !IO)
        ;
            io.write_char(' ', !IO),
            term_io.quote_atom(Functor, !IO),
            io.write_char(' ', !IO)
        ),
        adjust_priority_for_assoc(OpPriority, RightAssoc, RightPriority),
        io.do_write_univ(NonCanon, Arg2, RightPriority, !IO),
        maybe_write_paren(')', Priority, OpPriority, !IO)
    ;
        Args = [Arg1, Arg2],
        ops.lookup_binary_prefix_op(OpTable, Functor, OpPriority,
            FirstAssoc, SecondAssoc)
    ->
        maybe_write_paren('(', Priority, OpPriority, !IO),
        term_io.quote_atom(Functor, !IO),
        io.write_char(' ', !IO),
        adjust_priority_for_assoc(OpPriority, FirstAssoc, FirstPriority),
        io.do_write_univ(NonCanon, Arg1, FirstPriority, !IO),
        io.write_char(' ', !IO),
        adjust_priority_for_assoc(OpPriority, SecondAssoc, SecondPriority),
        io.do_write_univ(NonCanon, Arg2, SecondPriority, !IO),
        maybe_write_paren(')', Priority, OpPriority, !IO)
    ;
        (
            Args = [],
            ops.lookup_op(OpTable, Functor),
            Priority =< ops.max_priority(OpTable)
        ->
            io.write_char('(', !IO),
            term_io.quote_atom(Functor, !IO),
            io.write_char(')', !IO)
        ;
            term_io.quote_atom(Functor, maybe_adjacent_to_graphic_token, !IO)
        ),
        (
            Args = [X | Xs]
        ->
            io.write_char('(', !IO),
            io.write_arg(NonCanon, X, !IO),
            io.write_term_args(NonCanon, Xs, !IO),
            io.write_char(')', !IO)
        ;
            true
        )
    ).

adjust_priority_for_assoc(Priority, y, Priority).
adjust_priority_for_assoc(Priority, x, Priority - 1).

maybe_write_paren(Char, Priority, OpPriority, !IO) :-
    ( OpPriority > Priority ->
        io.write_char(Char, !IO)
    ;
        true
    ).

:- pred io.write_list_tail(deconstruct.noncanon_handling, univ, io, io).
:- mode io.write_list_tail(in(do_not_allow), in, di, uo) is det.
:- mode io.write_list_tail(in(canonicalize), in, di, uo) is det.
:- mode io.write_list_tail(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.write_list_tail(in, in, di, uo) is cc_multi.

io.write_list_tail(NonCanon, Univ, !IO) :-
    Term = univ_value(Univ),
    deconstruct.deconstruct(Term, NonCanon, Functor, _Arity, Args),
    (
        Functor = "[|]",
        Args = [ListHead, ListTail]
    ->
        io.write_string(", ", !IO),
        io.write_arg(NonCanon, ListHead, !IO),
        io.write_list_tail(NonCanon, ListTail, !IO)
    ;
        Functor = "[]",
        Args = []
    ->
        true
    ;
        io.write_string(" | ", !IO),
        io.do_write_univ(NonCanon, Univ, !IO)
    ).

    % Write the remaining arguments.
    %
:- pred io.write_term_args(deconstruct.noncanon_handling, list(univ),
    io, io).
:- mode io.write_term_args(in(do_not_allow), in, di, uo) is det.
:- mode io.write_term_args(in(canonicalize), in, di, uo) is det.
:- mode io.write_term_args(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.write_term_args(in, in, di, uo) is cc_multi.

io.write_term_args(_, [], !IO).
io.write_term_args(NonCanon, [X | Xs], !IO) :-
    io.write_string(", ", !IO),
    io.write_arg(NonCanon, X, !IO),
    io.write_term_args(NonCanon, Xs, !IO).

:- pred io.write_arg(deconstruct.noncanon_handling, univ, io, io).
:- mode io.write_arg(in(do_not_allow), in, di, uo) is det.
:- mode io.write_arg(in(canonicalize), in, di, uo) is det.
:- mode io.write_arg(in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.write_arg(in, in, di, uo) is cc_multi.

io.write_arg(NonCanon, X, !IO) :-
    arg_priority(ArgPriority, !IO),
    io.do_write_univ(NonCanon, X, ArgPriority, !IO).

:- pred arg_priority(int::out, io::di, io::uo) is det.

% arg_priority(ArgPriority, !IO) :-
%   io.get_op_table(OpTable, !IO),
%   ( ops.lookup_infix_op(OpTable, ",", Priority, _, _) ->
%       ArgPriority = Priority
%   ;
%       error("arg_priority: can't find the priority of `,'")
%   ).
%
% We could implement this as above, but it's more efficient to just
% hard-code it.
arg_priority(1000, !IO).

%-----------------------------------------------------------------------------%

:- pred io.write_type_desc(type_desc::in, io::di, io::uo) is det.

io.write_type_desc(TypeDesc, !IO) :-
    io.write_string(type_name(TypeDesc), !IO).

:- pred io.write_type_ctor_desc(type_ctor_desc::in, io::di, io::uo) is det.

io.write_type_ctor_desc(TypeCtorDesc, !IO) :-
    type_ctor_name_and_arity(TypeCtorDesc, ModuleName, Name, Arity0),
    (
        ModuleName = "builtin",
        Name = "func"
    ->
        % The type ctor that we call `builtin:func/N' takes N + 1
        % type parameters: N arguments plus one return value.
        % So we need to subtract one from the arity here.
        Arity = Arity0 - 1
    ;
        Arity = Arity0
    ),
    ( ModuleName = "builtin" ->
        io.format("%s/%d", [s(Name), i(Arity)], !IO)
    ;
        io.format("%s.%s/%d", [s(ModuleName), s(Name), i(Arity)], !IO)
    ).

:- pred io.write_c_pointer(c_pointer::in, io::di, io::uo) is det.

io.write_c_pointer(_C_Pointer, !IO) :-
    % XXX What should we do here?
    io.write_string("'<<c_pointer>>'", !IO).

:- pred io.write_array(array(T)::in, io::di, io::uo) is det.

io.write_array(Array, !IO) :-
    io.write_string("array(", !IO),
    array.to_list(Array, List),
    io.write(List, !IO),
    io.write_string(")", !IO).

:- pred io.write_private_builtin_type_info(private_builtin.type_info::in,
    io::di, io::uo) is det.

io.write_private_builtin_type_info(PrivateBuiltinTypeInfo, !IO) :-
    TypeInfo = rtti_implementation.unsafe_cast(PrivateBuiltinTypeInfo),
    io.write_type_desc(TypeInfo, !IO).

%-----------------------------------------------------------------------------%

io.write_list([], _Separator, _OutputPred, !IO).
io.write_list([E | Es], Separator, OutputPred, !IO) :-
    call(OutputPred, E, !IO),
    (
        Es = []
    ;
        Es = [_ | _],
        io.write_string(Separator, !IO)
    ),
    io.write_list(Es, Separator, OutputPred, !IO).

io.write_list(Stream, List, Separator, OutputPred, !IO) :-
    io.set_output_stream(Stream, OrigStream, !IO),
    io.write_list(List, Separator, OutputPred, !IO),
    io.set_output_stream(OrigStream, _Stream, !IO).

%-----------------------------------------------------------------------------%

io.write_binary(Stream, Term, !IO) :-
    io.set_binary_output_stream(Stream, OrigStream, !IO),
    io.write_binary(Term, !IO),
    io.set_binary_output_stream(OrigStream, _Stream, !IO).

io.read_binary(Stream, Result, !IO) :-
    io.set_binary_input_stream(Stream, OrigStream, !IO),
    io.read_binary(Result, !IO),
    io.set_binary_input_stream(OrigStream, _Stream, !IO).

io.write_binary(Term, !IO) :-
    % A quick-and-dirty implementation... not very space-efficient
    % (not really binary!)
    % XXX This will not work for the Java back-end. See the comment at the
    % top of the MR_MercuryFileStruct class definition.
    io.binary_output_stream(Stream, !IO),
    io.write(Stream, Term, !IO),
    io.write_string(Stream, ".\n", !IO).

io.read_binary(Result, !IO) :-
    % A quick-and-dirty implementation... not very space-efficient
    % (not really binary!)
    % XXX This will not work for the Java back-end. See the comment at the
    % top of the MR_MercuryFileStruct class definition.
    io.binary_input_stream(Stream, !IO),
    io.read(Stream, ReadResult, !IO),
    (
        ReadResult = ok(T),
        % We've read the newline and the trailing full stop.
        % Now skip the newline after the full stop.
        io.read_char(Stream, NewLineRes, !IO),
        ( NewLineRes = error(Error) ->
            Result = error(Error)
        ; NewLineRes = ok('\n') ->
            Result = ok(T)
        ;
            Result = error(io_error("io.read_binary: missing newline"))
        )
    ;
        ReadResult = eof,
        Result = eof
    ;
        ReadResult = error(ErrorMsg, _Line),
        Result = error(io_error(ErrorMsg))
    ).

%-----------------------------------------------------------------------------%

% stream predicates

io.open_input(FileName, Result, !IO) :-
    io.do_open_text(FileName, "r", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(NewStream),
        io.insert_stream_info(NewStream,
            stream(OpenCount, input, text, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open input file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_output(FileName, Result, !IO) :-
    io.do_open_text(FileName, "w", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(NewStream),
        io.insert_stream_info(NewStream,
            stream(OpenCount, output, text, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open output file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_append(FileName, Result, !IO) :-
    io.do_open_text(FileName, "a", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(NewStream),
        io.insert_stream_info(NewStream,
            stream(OpenCount, append, text, file(FileName)), !IO)
    ;
        io.make_err_msg("can't append to file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_binary_input(FileName, Result, !IO) :-
    io.do_open_binary(FileName, "rb", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(NewStream),
        io.insert_stream_info(NewStream,
            stream(OpenCount, input, binary, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open input file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_binary_output(FileName, Result, !IO) :-
    io.do_open_binary(FileName, "wb", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(NewStream),
        io.insert_stream_info(NewStream,
            stream(OpenCount, output, binary, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open output file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_binary_append(FileName, Result, !IO) :-
    io.do_open_binary(FileName, "ab", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(NewStream),
        io.insert_stream_info(NewStream,
            stream(OpenCount, append, binary, file(FileName)), !IO)
    ;
        io.make_err_msg("can't append to file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

%-----------------------------------------------------------------------------%

% Declarative versions of Prolog's see/1 and seen/0.

io.see(File, Result, !IO) :-
    io.open_input(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_input_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

io.seen(!IO) :-
    io.stdin_stream(Stdin, !IO),
    io.set_input_stream(Stdin, OldStream, !IO),
    io.close_input(OldStream, !IO).

% Plus binary IO versions.

io.see_binary(File, Result, !IO) :-
    io.open_binary_input(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_binary_input_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

io.seen_binary(!IO) :-
    io.stdin_binary_stream(Stdin, !IO),
    io.set_binary_input_stream(Stdin, OldStream, !IO),
    io.close_binary_input(OldStream, !IO).

%-----------------------------------------------------------------------------%

% Declarative versions of Prolog's tell/1 and told/0.

io.told(!IO) :-
    io.stdout_stream(Stdout, !IO),
    io.set_output_stream(Stdout, OldStream, !IO),
    io.close_output(OldStream, !IO).

io.tell(File, Result, !IO) :-
    io.open_output(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_output_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

io.told_binary(!IO) :-
    io.stdout_binary_stream(Stdout, !IO),
    io.set_binary_output_stream(Stdout, OldStream, !IO),
    io.close_binary_output(OldStream, !IO).

io.tell_binary(File, Result, !IO) :-
    io.open_binary_output(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_binary_output_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Stream name predicates.

io.input_stream_name(Name, !IO) :-
    io.input_stream(Stream, !IO),
    io.stream_name(Stream, Name, !IO).

io.input_stream_name(Stream, Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

io.output_stream_name(Name, !IO) :-
    io.output_stream(Stream, !IO),
    io.stream_name(Stream, Name, !IO).

io.output_stream_name(Stream, Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

io.binary_input_stream_name(Name, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.stream_name(Stream, Name, !IO).

io.binary_input_stream_name(Stream, Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

io.binary_output_stream_name(Name, !IO) :-
    io.binary_output_stream(Stream, !IO),
    io.stream_name(Stream, Name, !IO).

io.binary_output_stream_name(Stream, Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

:- pred io.stream_name(io.stream::in, string::out, io::di, io::uo) is det.

io.stream_name(Stream, Name, !IO) :-
    io.stream_info(Stream, MaybeInfo, !IO),
    (
        MaybeInfo = yes(Info),
        Info = stream(_, _, _, Source),
        Name = source_name(Source)
    ;
        MaybeInfo = no,
        Name = "<stream name unavailable>"
    ).

:- pred io.stream_info(io.stream::in, maybe(stream_info)::out,
    io::di, io::uo) is det.

io.stream_info(Stream, MaybeInfo, !IO) :-
    io.get_stream_db(StreamDb, !IO),
    ( map.search(StreamDb, get_stream_id(Stream), Info) ->
        MaybeInfo = yes(Info)
    ;
        MaybeInfo = no
    ).

io.input_stream_info(StreamDb, Stream) =
    io.maybe_stream_info(StreamDb, Stream).

io.output_stream_info(StreamDb, Stream) =
    io.maybe_stream_info(StreamDb, Stream).

io.binary_input_stream_info(StreamDb, Stream) =
    io.maybe_stream_info(StreamDb, Stream).

io.binary_output_stream_info(StreamDb, Stream) =
    io.maybe_stream_info(StreamDb, Stream).

:- func io.maybe_stream_info(io.stream_db, io.stream) = maybe_stream_info.

io.maybe_stream_info(StreamDb, Stream) = Info :-
    ( map.search(StreamDb, get_stream_id(Stream), Info0) ->
        % Info0 and Info have different types.
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
    io.get_stream_db(StreamDb::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = ML_io_stream_db;
    MR_update_io(IO0, IO);
").

:- pred io.set_stream_db(io.stream_db::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.set_stream_db(StreamDb::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ML_io_stream_db = StreamDb;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
    io.get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = ML_io_stream_db;
").

:- pragma foreign_proc("C#",
    io.set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ML_io_stream_db = StreamDb;
").

:- pragma foreign_proc("Java",
    io.get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = ML_io_stream_db;
").

:- pragma foreign_proc("Java",
    io.set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ML_io_stream_db = StreamDb;
").

%-----------------------------------------------------------------------------%

:- pred io.insert_stream_info(io.stream::in, stream_info::in,
    io::di, io::uo) is det.

io.insert_stream_info(Stream, Name, !IO) :-
    io.get_stream_db(StreamDb0, !IO),
    map.set(StreamDb0, get_stream_id(Stream), Name, StreamDb),
    io.set_stream_db(StreamDb, !IO).

:- pred io.maybe_delete_stream_info(io.stream::in, io::di, io::uo) is det.

io.maybe_delete_stream_info(Stream, !IO) :-
    io.may_delete_stream_info(MayDeleteStreamInfo, !IO),
    ( MayDeleteStreamInfo \= 0 ->
        io.get_stream_db(StreamDb0, !IO),
        map.delete(StreamDb0, get_stream_id(Stream), StreamDb),
        io.set_stream_db(StreamDb, !IO)
    ;
        true
    ).

    % Return an integer that is nonzero if and only if we should delete
    % the information we have about stream when that stream is closed.
    % The debugger may need this information in order to display the stream id
    % in a user-friendly manner even after the stream is closed (e.g. after
    % performing a retry after the close), so if debugging is enabled, we
    % hang on to the stream info until the end of the execution. This is a
    % space leak, but one that is acceptable in a program being debugged.
    %
:- pred io.may_delete_stream_info(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.may_delete_stream_info(MayDelete::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    MayDelete = !MR_debug_ever_enabled;
    IO = IO0;
").

io.may_delete_stream_info(1, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Global state predicates.

    % XXX design flaw with regard to unique modes
    % and io.get_globals/3: the `Globals::uo' mode here is a lie.

:- pragma foreign_proc("C",
    io.get_globals(Globals::uo, IOState0::di, IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = ML_io_user_globals;
    MR_update_io(IOState0, IOState);
").

:- pragma foreign_proc("C",
    io.set_globals(Globals::di, IOState0::di, IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* XXX need to globalize the memory */
    ML_io_user_globals = Globals;
    MR_update_io(IOState0, IOState);
").

:- pragma foreign_proc("C#",
    io.get_globals(Globals::uo, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = ML_io_user_globals;
").

:- pragma foreign_proc("C#",
    io.set_globals(Globals::di, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ML_io_user_globals = Globals;
").

:- pragma foreign_proc("Java",
    io.get_globals(Globals::uo, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = ML_io_user_globals;
").

:- pragma foreign_proc("Java",
    io.set_globals(Globals::di, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ML_io_user_globals = Globals;
").

io.progname_base(DefaultName, PrognameBase, !IO) :-
    io.progname(DefaultName, Progname, !IO),
    PrognameBase = dir.basename_det(Progname).

:- pragma foreign_proc("C",
    io.get_stream_id(Stream::in) = (Id::out),
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
    ** For accurate GC we embed an ID in the MercuryFile
    ** and retrieve it here.
    */
    Id = (Stream)->id;
#endif
").

:- pragma foreign_proc("C#",
    io.get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure],
"
    Id = Stream.id;
").

:- pragma foreign_proc("Java",
    io.get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure],
"
    Id = Stream.id;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% environment interface predicates

:- pragma promise_pure(io.get_environment_var/4).

io.get_environment_var(Var, OptValue, !IO) :-
    ( semipure io.getenv(Var, Value) ->
        OptValue0 = yes(Value)
    ;
        OptValue0 = no
    ),
    OptValue = OptValue0.

:- pragma promise_pure(io.set_environment_var/4).

io.set_environment_var(Var, Value, !IO) :-
    ( impure io.setenv(Var, Value) ->
        true
    ;
        string.format("Could not set environment variable `%s'",
            [s(Var)], Message),
        error(Message)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Statistics reporting predicates.

io.report_stats(!IO) :-
    io.report_stats("standard", !IO).

:- pragma promise_pure(io.report_stats/3).

io.report_stats(Selector, !IO) :-
    ( Selector = "standard" ->
        impure report_stats
    ; Selector = "full_memory_stats" ->
        impure report_full_memory_stats
    ; Selector = "tabling" ->
        impure table_builtin.table_report_statistics
    ;
        string.format("io.report_stats: selector `%s' not understood",
            [s(Selector)], Message),
        error(Message)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% miscellaneous predicates

:- interface.

    % XXX Since on the IL backend pragma export is NYI, this
    % predicate must be placed in the interface.
    %
:- pred io.init_state(io::di, io::uo) is det.

:- implementation.

    % For use by the Mercury runtime.
:- pragma export(io.init_state(di, uo), "ML_io_init_state").

io.init_state(!IO) :-
    io.gc_init(type_of(StreamDb), type_of(Globals), !IO),
    map.init(StreamDb),
    type_to_univ("<globals>", Globals),
    io.set_stream_db(StreamDb, !IO),
    io.set_op_table(ops.init_mercury_op_table, !IO),
    io.set_globals(Globals, !IO),
    io.insert_std_stream_names(!IO).

:- pred io.finalize_state(io::di, io::uo) is det.

    % For use by the Mercury runtime.
:- pragma export(io.finalize_state(di, uo), "ML_io_finalize_state").

    % Currently no finalization needed...
    % (Perhaps we should close all open Mercury files?
    % That will happen on process exit anyway, so currently we don't bother.)
io.finalize_state(!IO).

:- pred io.gc_init(type_desc::in, type_desc::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.gc_init(StreamDbType::in, UserGlobalsType::in, IO0::di, IO::uo),
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

io.gc_init(_, _, !IO).

:- pred io.insert_std_stream_names(io::di, io::uo) is det.

io.insert_std_stream_names(!IO) :-
    io.stdin_stream(Stdin, !IO),
    io.insert_stream_info(Stdin, stream(0, input, preopen, stdin), !IO),
    io.stdout_stream(Stdout, !IO),
    io.insert_stream_info(Stdout, stream(1, output, preopen, stdout), !IO),
    io.stderr_stream(Stderr, !IO),
    io.insert_stream_info(Stderr, stream(1, output, preopen, stderr), !IO).

io.call_system(Command, Result, !IO) :-
    io.call_system_return_signal(Command, Result0, !IO),
    (
        Result0 = ok(exited(Code)),
        Result = ok(Code)
    ;
        Result0 = ok(signalled(Signal)),
        string.int_to_string(Signal, SignalStr),
        ErrMsg = "system command killed by signal number " ++ SignalStr,
        Result = error(io_error(ErrMsg))
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

io.call_system_return_signal(Command, Result, !IO) :-
    io.call_system_code(Command, Code, Msg, !IO),
    ( Code = 127 ->
        Result = error(io_error(Msg))
    ;
        Result = io.handle_system_command_exit_status(Code)
    ).

:- type io.error
    --->    io_error(string).       % This is subject to change.
    % Note that we use `io_error' rather than `io.error' because io.print,
    % which may be called to print out the uncaught exception if there is no
    % exception hander, does not print out the module name.

io.make_io_error(Error) = io_error(Error).

io.error_message(io_error(Error), Error).

%-----------------------------------------------------------------------------%

    % XXX design flaw with regard to unique modes and
    % io.get_op_table

io.get_op_table(ops.init_mercury_op_table, !IO).

io.set_op_table(_OpTable, !IO).

%-----------------------------------------------------------------------------%

% For use by the debugger:

:- pred io.get_io_input_stream_type(type_desc::out, io::di, io::uo) is det.

:- pragma export(io.get_io_input_stream_type(out, di, uo),
    "ML_io_input_stream_type").

io.get_io_input_stream_type(Type, !IO) :-
    io.stdin_stream(Stream, !IO),
    Type = type_of(Stream).

:- pred io.get_io_output_stream_type(type_desc::out, io::di, io::uo) is det.

:- pragma export(io.get_io_output_stream_type(out, di, uo),
    "ML_io_output_stream_type").

io.get_io_output_stream_type(Type, !IO) :-
    io.stdout_stream(Stream, !IO),
    Type = type_of(Stream).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The remaining predicates are implemented using the C interface.

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
  #include <sys/wait.h>     /* for WIFEXITED, WEXITSTATUS, etc. */
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

#define MR_initial_io_state()       0   /* some random number */
#define MR_final_io_state(r)        ((void)0)

#define MR_update_io(r_src, r_dest) ((r_dest) = (r_src))

void            mercury_init_io(void);
MercuryFilePtr  mercury_open(const char *filename, const char *openmode);
void            mercury_io_error(MercuryFilePtr mf, const char *format, ...);
void            mercury_output_error(MercuryFilePtr mf);
void            mercury_print_string(MercuryFilePtr mf, const char *s);
void            mercury_print_binary_string(MercuryFilePtr mf, const char *s);
int             mercury_getc(MercuryFilePtr mf);
void            mercury_close(MercuryFilePtr mf);
int             ML_fprintf(MercuryFilePtr mf, const char *format, ...);
").

:- pragma foreign_decl("C#", "

namespace mercury {
  namespace io__csharp_code {
    public enum ML_file_encoding_kind {
        ML_OS_text_encoding,    // file stores characters,
                                // using the operating system's
                                // default encoding, and OS's
                                // usual line-ending convention
                                // (e.g. CR-LF for DOS/Windows).

        ML_Unix_text_encoding,  // file stores characters,
                                // using the operating system's
                                // default encoding, but with the
                                // Unix line-ending convention.

        ML_raw_binary           // file stores bytes
    };

    public class MR_MercuryFileStruct {
        // Note that stream reader and writer are initialized lazily;
        // that is, if the stream has not yet been used for reading,
        // the `reader' field may be null.  Any code which accesses that
        // field must check for null and initialize it if needed.
        // Likewise for the `writer' field.

        public System.IO.Stream     stream; // The stream itself
        public System.IO.TextReader reader; // The stream reader for it
        public System.IO.TextWriter writer; // The stream write for it
        public int                  putback;
                                    // the next character or byte to read,
                                    // or -1 if no putback char/byte is stored

        public ML_file_encoding_kind    file_encoding;
                                    // DOS, Unix, or raw binary

        public int                  line_number;
        public int                  id;
    };
  }
}
").

:- pragma foreign_code("Java",
"
    /* All text files (including stdin/stdout/stderr) are run through
    ** stream readers/writers, which use the default charset encoding. In
    ** other words, while strings and characters are stored internally
    ** using unicode, all files are read and written using the default
    ** system charset.
    ** Binary files are opened via RandomAccessFile, which supports seek,
    ** but not character encoding/decoding or buffering.
    ** Binary stdin and stdout are a special case.  They are opened via
    ** FileInput/OutputStreams and for Java versions >= 1.4, seeking is
    ** controlled through use of FileChannels. (obtained by Reflection so
    ** as not to break the compilation using earlier versions of Java)
    **
    ** The use of the methods in this implementation is not very flexible.
    ** You may not perform text mode operations on a binary file or vice
    ** versa.
    ** XXX This causes problems for io.read_binary and io.write_binary,
    ** for which the current implementations attempt to access binary
    ** streams using text mode operations (and so will definitely break.)
    */

    public static class MR_MercuryFileStruct {
        public  static final int            INPUT       = 0;
        public  static final int            OUTPUT      = 1;
        private int                         mode;

        private static final int            SEEK_SET    = 0;
        private static final int            SEEK_CUR    = 1;
        private static final int            SEEK_END    = 2;

        public  static int                  ML_next_stream_id = 0;
        public  int                         id;

        // line_number is only valid for text streams
        public  int                         line_number = 1;

        // pushback is non-null only for input streams
        private java.util.Stack             pushback    = null;

        // input is non-null only for text input streams
        private java.io.InputStreamReader   input       = null;

        // output is non-null only for text output streams
        private java.io.OutputStreamWriter  output      = null;

        // randomaccess is non-null only for binary streams
        // (excluding binary stdin and stdout)
        private java.io.RandomAccessFile    randomaccess    = null;

        // binary_input is non-null only for binary_stdin
        private java.io.FileInputStream     binary_input    = null;

        // binary_output is non-null only for binary_stdout
        private java.io.FileOutputStream    binary_output   = null;

        // For Java versions < 1.4, there is no way to retrieve
        // the current offset into binary stdin/out.
        // In this case, position records the current offset,
        // otherwise this field is not used for anything.
        int                                 position    = 0;

        // channel is non-null only for binary stdin/out using
        // Java versions >= 1.4 (those supporting FileChannel).
        private java.lang.Object            channel     = null;

        /*
        ** This constructor is for text input streams.
        */
        public MR_MercuryFileStruct(java.io.InputStream stream) {
            this(stream, false);
        }

        /*
        ** This constructor is for text output streams.
        */
        public MR_MercuryFileStruct(java.io.OutputStream stream) {
            this(stream, false);
        }

        /*
        ** This constructor handles binary files (in or out) but does
        ** not cover mercury_stdin_binary/mercury_stdout_binary.
        */
        public MR_MercuryFileStruct(java.lang.String file, char mode) {
            id = ML_next_stream_id++;
            String openstring;

            if (mode == 'r') {
                openstring = ""r"";
                this.mode = INPUT;
                pushback = new java.util.Stack();
            } else if (mode == 'w' || mode == 'a') {
                openstring = ""rw"";
                this.mode = OUTPUT;
                // There is no such mode as ""w"", which could be a problem
                // for write-only files.
            } else {
                throw new RuntimeException(""Invalid file opening mode"");
            }
            try {
                randomaccess = new java.io.RandomAccessFile(file, openstring);
                if (mode == 'a') {
                    seek(SEEK_END, 0);
                }
            } catch (java.lang.Exception e) {
                throw new RuntimeException(e.getMessage());
                // We can't just throw e or the Java compiler
                // will complain about unreported exceptions.
            }
        }

        public MR_MercuryFileStruct(java.io.InputStream stream,
            boolean openAsBinary)
        {
            id          = ML_next_stream_id++;
            mode        = INPUT;
            pushback    = new java.util.Stack();

            if (!openAsBinary) {
                input = new java.io.InputStreamReader(stream);
            } else {
                /* open stdin as binary */
                binary_input = new java.io.FileInputStream(
                    java.io.FileDescriptor.in);
                channel = get_channel(binary_input);
            }
        }

        public MR_MercuryFileStruct(java.io.OutputStream stream,
            boolean openAsBinary)
        {
            id      = ML_next_stream_id++;
            mode    = OUTPUT;

            if (!openAsBinary) {
                output = new java.io.OutputStreamWriter(stream);
            } else {
                /* open stdout as binary */
                binary_output = new java.io.FileOutputStream(
                    java.io.FileDescriptor.out);
                channel = get_channel(binary_output);
            }
        }

        /*
        ** size():
        **
        ** Returns the length of a binary file. XXX Note that this method
        ** will return -1 for mercury_stdin_binary and the current position
        ** for mercury_stdout_binary in Java versions < 1.4.
        */
        public int size() {
            if (randomaccess != null) {
                try {
                    return (int) randomaccess.length();
                } catch (java.io.IOException e) {
                    return -1;
                }
            }

            try {
                java.lang.reflect.Method size_mth =
                    channel.getClass().getMethod(""size"", null);
                return ((Long) size_mth.invoke(channel, null)).intValue();
            } catch (java.lang.Exception e) {
                if (binary_output != null) {
                    return position;
                } else {
                    return -1;
                }
            }
        }

        /*
        ** seek():
        **
        ** Seek relative to start, current position or end depending on the
        ** flag. This function only works for binary files.
        **
        ** The binary versions of stdin and stdout are treated specially
        ** as follows.
        **
        ** For Java versions >= 1.4:
        **  Reflection is used to obtain and use the necessary FileChannel
        **  object needed to perform seeking on each.
        ** For older versions:
        **  For mercury_stdin_binary, seek may be only be done forwards
        **  from the current position and for mercury_stdout_binary seek is not
        **  supported at all.
        */
        public void seek(int flag, int offset) {
            if (channel != null) {
                channelSeek(flag, offset);
                return;
            }

            if (input != null || output != null) {
                throw new java.lang.RuntimeException(
                    ""Java text streams are not seekable"");
            }
            if (binary_output != null) {
                throw new java.lang.RuntimeException(
                    ""for Java versions < 1.4, "" +
                    ""mercury_stdout_binary is not seekable"");
            }
            if (binary_input != null &&
                (flag != SEEK_CUR || offset < 0))
            {
                throw new java.lang.RuntimeException(
                    ""for Java versions < 1.4, "" +
                    ""mercury_stdin_binary may only seek forwards "" +
                    ""from the current position"");
            }

            pushback = new java.util.Stack();

            try {
                switch (flag) {
                    case SEEK_SET:
                        randomaccess.seek(offset);
                        break;
                    case SEEK_CUR:
                        if (randomaccess != null) {
                            randomaccess.seek(
                                randomaccess.getFilePointer() + offset);
                        } else {
                            binary_input.skip(offset);
                        }
                        break;
                    case SEEK_END:
                        randomaccess.seek(randomaccess.length() + offset);
                        break;
                    default:
                        throw new java.lang.
                            RuntimeException(""Invalid seek flag"");
                }
            } catch (java.lang.Exception e) {
                throw new java.lang.RuntimeException(e.getMessage());
            }
        }

        /*
        ** channelSeek():
        **
        ** Seek relative to start, current position or end depending on the
        ** flag. This function is a special case of seek() above, used when
        ** a FileChannel is present and usable.  At present, this is only the
        ** case for binary stdin/out using Java versions >= 1.4. FileChannel's
        ** position() method must be called using Reflection so that this code
        ** will still compile for Java versions < 1.4.
        */
        private void channelSeek(int flag, int offset) {
            pushback = new java.util.Stack();

            try {
                switch (flag) {
                    case SEEK_SET:
                        position = offset;
                        break;
                    case SEEK_CUR:
                        position = getOffset() + offset;
                        break;
                    case SEEK_END:
                        position = size() + offset;
                        break;
                    default:
                        throw new java.lang.
                            RuntimeException(""Invalid seek flag"");
                }

                // Simulate
                //  channel.position(position);
                // using reflection.
                Class[] argTypes = {Long.TYPE};
                java.lang.reflect.Method seek_mth =
                    channel.getClass().getMethod(""position"", argTypes);

                Object[] args = {new Long(position)};
                seek_mth.invoke(channel, args);
            }
            catch (java.lang.Exception e) {
                throw new java.lang.RuntimeException(e.getMessage());
            }
        }

        /*
        ** getOffset():
        **  Returns the current position in a binary file.
        */
        public int getOffset() {
            if (randomaccess != null) {
                try {
                    return (int) randomaccess.getFilePointer();
                } catch (java.io.IOException e) {
                    return -1;
                }
            }

            try {
                java.lang.reflect.Method posn_mth =
                    channel.getClass().getMethod(""position"", null);
                return ((Long) posn_mth.invoke(channel, null)).intValue();
            } catch (java.lang.Exception e) {
                if (binary_input != null || binary_output != null) {
                    return position;
                } else {
                    return -1;
                }
            }
        }

        /*
        ** read_char():
        **
        ** Reads one character in from a text input file using the default
        ** charset decoding.  Returns -1 at end of file.
        */
        public int read_char() {
            int c;
            if (input == null) {
                throw new java.lang.RuntimeException(
                    ""read_char_code may only be called"" +
                    "" on text input streams"");
            }
            if (pushback.empty()) {
                try {
                    c = input.read();
                } catch (java.io.IOException e) {
                    throw new java.lang.RuntimeException(e.getMessage());
                }
            } else {
                c = ((java.lang.Integer)pushback.pop()).intValue();
            }

            if (c == '\\n') {
                line_number++;
            }

            return c;
        }

        /*
        ** read_byte():
        **
        ** Reads one byte in from a binary file. Returns -1 at end of file.
        */
        public int read_byte() {
            int c;
            if (mode == OUTPUT) {
                throw new java.lang.RuntimeException(
                    ""Attempted to read output stream"");
            }
            if (randomaccess == null && binary_input == null) {
                throw new java.lang.RuntimeException(
                    ""read_byte_val may only be called"" +
                    "" on binary input streams"");
            }
            if (pushback.empty()) {
                try {
                    if (binary_input != null) {
                        c = binary_input.read();
                    } else {
                        c = randomaccess.read();
                    }
                } catch (java.io.IOException e) {
                    throw new java.lang.RuntimeException(e.getMessage());
                }
            } else {
                c = ((java.lang.Integer)pushback.pop()).intValue();
            }
            position++;

            return c;
        }

        /*
        ** ungetc():
        **
        ** Pushes an integer, which may represent either a byte or a character,
        ** onto the pushback stack. This stack is the same, regardless of
        ** whether the file is text or binary.
        */
        public void ungetc(int c) {
            if (mode == OUTPUT) {
                throw new java.lang.RuntimeException(
                    ""Attempted to unget char to output stream"");
            }
            if (c == '\\n') {
                line_number--;
            }

            pushback.push(new Integer(c));
            position--;
        }

        /*
        ** put():
        **
        ** Write one unit to an output stream.  If the file is text, the int
        ** will hold a character. If the file is binary, this will be a
        ** single byte. In the former case we assume that the lower order
        ** 16 bits hold a char, in latter we take only the lowest 8 bits
        ** for a byte.
        */
        public void put(int c) {
            if (mode == INPUT) {
                throw new java.lang.RuntimeException(
                    ""Attempted to write to input stream"");
            }
            if (c == '\\n') {
                line_number++;
            }

            try {
                if (output != null) {
                    output.write(c);
                } else if (randomaccess != null) {
                    randomaccess.write(c);
                } else {
                    binary_output.write(c);
                    position++;
                }
            } catch (java.io.IOException e) {
                throw new java.lang.RuntimeException(e.getMessage());
            }
        }

        /*
        ** write():
        **
        ** Writes a string to a file stream.  For text files, this string
        ** is encoded as a byte stream using default encoding. For binary
        ** files, the lower order 8 bits of each character are written.
        */
        public void write(java.lang.String s) {
            if (mode == INPUT) {
                throw new java.lang.RuntimeException(
                    ""Attempted to write to input stream"");
            }

            try {
                if (output != null) {
                    for (int i = 0; i < s.length(); i++) {
                        if (s.charAt(i) == '\\n') {
                            line_number++;
                        }
                    }
                    output.write(s);
                } else {
                    for (int i = 0; i < s.length(); i++) {
                        // lower 8 bits of each
                        put((byte) s.charAt(i));
                    }
                }
            } catch (java.io.IOException e) {
                throw new java.lang.RuntimeException(e.getMessage());
            }
        }

        public void flush() {
            if (mode == INPUT) {
                throw new java.lang.RuntimeException(
                    ""Attempted to flush input stream"");
            }

            try {
                if (output != null) {
                    output.flush();
                }
                if (binary_output != null) {
                    binary_output.flush();
                }
                // else randomaccess is already unbuffered.
            } catch (java.io.IOException e) {
                throw new java.lang.RuntimeException(e.getMessage());
            }
        }

        public void close() {
            try {
                if (input != null) {
                    input.close();
                }
                if (output != null) {
                    output.close();
                }
                if (randomaccess != null) {
                    randomaccess.close();
                }
                if (binary_input != null) {
                    binary_input.close();
                }
                if (binary_output != null) {
                    binary_output.close();
                }
            } catch (java.io.IOException e) {
                throw new java.lang.RuntimeException(""Error closing stream"");
            }
        }
    } // class MR_MercuryFileStruct

    // StreamPipe is a mechanism for connecting streams to those of a
    // Runtime.exec() Process.

    private static class StreamPipe extends java.lang.Thread {
        MR_MercuryFileStruct    in;
        MR_MercuryFileStruct    out;
        boolean                 closeOutput = false;
        java.lang.Exception     exception = null;

        StreamPipe(java.io.InputStream in, MR_MercuryFileStruct out) {
            this.in  = new MR_MercuryFileStruct(in);
            this.out = out;
        }

        StreamPipe(MR_MercuryFileStruct in, java.io.OutputStream out) {
            this.in  = in;
            this.out = new MR_MercuryFileStruct(out);
            closeOutput = true;
        }

        public void run() {
            try {
                while (true) {
                    int b = in.read_char();
                    if (b == -1 || interrupted()) {
                        break;
                    }
                    out.put(b);
                }
                out.flush();
                if (closeOutput) {
                    out.close();
                }
            }
            catch (java.lang.Exception e) {
                exception = e;
            }
        }
    } // class StreamPipe

    /*
    ** get_channel():
    **
    ** Given some object, attempts to call getChannel() using Reflection
    ** and returns the result, or null if getChannel() is not available.
    ** The reason we do this is that FileChannels were not supported until
    ** Java v1.4, so users with older versions of Java would not be able to
    ** compile this code if the call to getChannel were made directly.
    ** This way, older versions of Java will still be able to compile,
    ** but will not support binary seeking on stdin/out properly, since
    ** this can't be done without FileChannel.
    */
    static java.lang.Object/*FileChannel*/ get_channel(java.lang.Object o)
    {
        try {
            // Simulate
            //  return o.getChannel();
            // using reflection.
            java.lang.reflect.Method getChannel_mth =
                o.getClass().getMethod(""getChannel"", null);

            return getChannel_mth.invoke(o, null);
        }
        catch (java.lang.Exception e) {
            return null;
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

    // Note: for Windows GUI programs, the Console is set to the equivalent
    // of /dev/null.  This could perhaps be considered a problem. But if so,
    // it is a problem in Windows, not in Mercury -- I don't think it is one
    // that the Mercury implementation should try to solve.

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

static MR_MercuryFileStruct mercury_current_text_input =
    mercury_stdin;
static MR_MercuryFileStruct mercury_current_text_output =
    mercury_stdout;
static MR_MercuryFileStruct mercury_current_binary_input =
    mercury_stdin_binary;
static MR_MercuryFileStruct mercury_current_binary_output =
    mercury_stdout_binary;

// XXX not thread-safe!
static System.Exception MR_io_exception;

").

:- pragma foreign_code("Java",
"
static MR_MercuryFileStruct mercury_stdin =
    new MR_MercuryFileStruct(java.lang.System.in);
static MR_MercuryFileStruct mercury_stdout =
    new MR_MercuryFileStruct(java.lang.System.out);
static MR_MercuryFileStruct mercury_stderr =
    new MR_MercuryFileStruct(java.lang.System.err);
static MR_MercuryFileStruct mercury_stdin_binary =
    new MR_MercuryFileStruct(java.lang.System.in, true);
static MR_MercuryFileStruct mercury_stdout_binary =
    new MR_MercuryFileStruct(java.lang.System.out, true);

static MR_MercuryFileStruct mercury_current_text_input =
    mercury_stdin;
static MR_MercuryFileStruct mercury_current_text_output =
    mercury_stdout;
static MR_MercuryFileStruct mercury_current_binary_input =
    mercury_stdin_binary;
static MR_MercuryFileStruct mercury_current_binary_output =
    mercury_stdout_binary;

// XXX not thread-safe!
static java.lang.Exception MR_io_exception;
").

:- pragma foreign_code("C", "

MercuryFilePtr
mercury_open(const char *filename, const char *openmode)
{
    MercuryFilePtr  mf;
    FILE            *f;

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
    System.IO.FileMode      mode;
    System.IO.FileAccess    access;
    System.IO.FileShare     share;
    System.IO.Stream        stream = null;

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
        return mercury_file_init(new System.IO.BufferedStream(stream),
            null, null, file_encoding);
    }
}

").

:- pred throw_io_error(string::in) is erroneous.
:- pragma export(throw_io_error(in), "ML_throw_io_error").

throw_io_error(Message) :-
    throw(io_error(Message)).

:- pragma foreign_code("C", "

void
mercury_io_error(MercuryFilePtr mf, const char *format, ...)
{
    va_list     args;
    char        message[5000];
    MR_String   message_as_mercury_string;

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
    ML_throw_io_error(message_as_mercury_string);
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
// io.write_char, which (for efficiency) uses its own inline
// code, rather than calling this function.
static void
mercury_print_string(MR_MercuryFileStruct mf, string s)
{
    //
    // For the .NET back-end, strings are represented as Unicode. Text output
    // streams (which may be connected to text files, or to the console)
    // require a byte stream. This raises the question: how should we convert
    // from Unicode to the byte sequence?
    //
    // We leave this up to the system, by just using the TextWriter associated
    // with the file. For the console, this will be System.Console.Out, which
    // will use whatever encoding is appropriate for the console. For a file,
    // the TextWriter will use the System.Encoding.Default encoding, which
    // will normally be an 8-bit national character set. If the Unicode string
    // contains characters which can't be represented in this set, then the
    // encoder will throw an exception.
    //
    // For files, we construct the TextWriter here, rather than at file open
    // time, so that we don't try to construct TextWriters for input streams.

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
        // We can't just use the System.TextWriter.Write(String) method,
        // since that method doesn't convert newline characters to the
        // system's newline convention (e.g. CR-LF on Windows).
        // Only the WriteLine(...) method handles those properly.
        // So we have to output each character separately.

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
    // been obtained using `enum.from_int' (i.e. the reverse mode of
    // `char.to_int'), then probably it would be better to just
    // take the lower 8 bits of the Unicode values, and throw an
    // exception if any of the other bits are set.
    //
    // The documentation for io.write_bytes doesn't make it clear
    // which of these is the case.  It says ``the bytes are taken
    // from a string'', but it doesn't say how.  I will assume
    // that it means the bottom 8 bits of the Unicode value,
    // just like io.write_byte takes the byte from the bottom 8 bits
    // of the int value.

// XXX possible alternative implementation.
//  byte[] byte_array = System.Text.Encoding.Default().GetBytes(s);

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

// Read in a character. This means reading in one or more bytes,
// converting the bytes from the system's default encoding to Unicode,
// and possibly converting CR-LF to newline. Returns -1 on error or EOF.

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
                // the input file was ill-formed, e.g. it contained only raw
                // LFs rather than CR-LF. Perhaps we should throw an exception?
                // If not, we still need to treat this as a newline, and thus
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
                if (mf.reader.Peek() == System.Environment.NewLine[1]) {
                    mf.reader.Read();
                    mf.line_number++;
                    c = '\\n';
                } else if (c == '\\n') {
                    // the input file was ill-formed, e.g. it contained only
                    // raw CRs rather than CR-LF. Perhaps we should throw an
                    // exception? If not, we still need to treat this
                    // as a newline, and thus increment the line counter.
                    mf.line_number++;
                }
                break;
            default:
                mercury.runtime.Errors.SORRY(
                    ""mercury_getc: Environment.NewLine.Length"" +
                    ""is neither 1 nor 2"");
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
  #define MR_CLOSED_FILE_ERROR  EBADF
#else
  /* ANSI/ISO C guarantees that EDOM will exist */
  #define MR_CLOSED_FILE_ERROR  EDOM
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
    return -1;  /* XXX should this be 0? */
}

static int
ME_closed_stream_write(MR_StreamInfo *info, const void *buffer, size_t size)
{
    errno = MR_CLOSED_FILE_ERROR;
    return -1;  /* XXX should this be 0? */
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
    /* stream_type  = */    MR_USER_STREAM,
    /* stream_info  = */    { NULL },
    /* line_number  = */    0,

    /* close    = */    ME_closed_stream_close,
    /* read     = */    ME_closed_stream_read,
    /* write    = */    ME_closed_stream_write,

    /* flush    = */    ME_closed_stream_flush,
    /* ungetc   = */    ME_closed_stream_ungetch,
    /* getc     = */    ME_closed_stream_getch,
    /* vprintf  = */    ME_closed_stream_vfprintf,
    /* putc     = */    ME_closed_stream_putch
};

#endif /* MR_NEW_MERCURYFILE_STRUCT */

void
mercury_close(MercuryFilePtr mf)
{
    if (MR_CLOSE(*mf) < 0) {
        mercury_io_error(mf, ""error closing file: %s"", strerror(errno));
    }

#ifdef MR_NEW_MERCURYFILE_STRUCT

    /*
    ** MR_closed_stream is a dummy stream object containing pointers to
    ** functions that always return an error indication. Doing this ensures
    ** that future accesses to the file will fail nicely.
    */
    /*
    ** gcc 2.95.2 barfs on `*mf = MR_closed_stream;'
    ** so we use MR_memcpy() instead.
    */
    MR_memcpy(mf, &MR_closed_stream, sizeof(*mf));

    /*
    ** XXX It would be nice to have an autoconf check for the GNU libc
    ** function fopencookie(); we could use that to do a similar thing to what
    ** we do in the MR_NEW_MERCURYFILE_STRUCT case.
    */

/****
#elif defined(HAVE_FOPENCOOKIE)
    MR_file(*mf) = MR_closed_file;
****/

#else

    /*
    ** We want future accesses to the file to fail nicely. Ideally they would
    ** throw an exception, but that would require a check at every I/O
    ** operation, and for simple operations like putchar() or getchar(),
    ** that would be too expensive. Instead we just set the file pointer
    ** to NULL; on systems which trap null pointer dereferences, or if
    ** library/io.m is compiled with MR_assert assertions enabled
    ** (i.e. -DMR_LOWLEVEL_DEBUG), this will ensure that accessing closed files
    ** traps immediately rather than causing problems at some later point.
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
        ** For the accurate GC or no GC cases, we need to explicitly deallocate
        ** the memory here, to avoid a memory leak. Note that the accurate
        ** collector won't reclaim io_streams, since the io.stream type
        ** is defined as a foreign_type.
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
    int     rc;
    va_list args;

    va_start(args, format);
    rc = MR_VFPRINTF(*mf, format, args);
    va_end(args);

    return rc;
}

").

/* input predicates */

:- pragma foreign_proc("C",
    io.read_char_code(File::in, CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CharCode = mercury_getc(File);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.read_byte_val(File::in, ByteVal::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ByteVal = mercury_getc(File);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.putback_char(File::in, Character::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, terminates],
"{
    MercuryFilePtr mf = File;
    if (Character == '\\n') {
        MR_line_number(*mf)--;
    }
    /* XXX should work even if ungetc() fails */
    if (MR_UNGETCH(*mf, Character) == EOF) {
        mercury_io_error(mf, ""io.putback_char: ungetc failed"");
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.putback_byte(File::in, Character::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, terminates],
"{
    MercuryFilePtr mf = File;
    /* XXX should work even if ungetc() fails */
    if (MR_UNGETCH(*mf, Character) == EOF) {
        mercury_io_error(mf, ""io.putback_byte: ungetc failed"");
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.read_char_code(File::in, CharCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    MR_MercuryFileStruct mf = File;
    CharCode = mercury_getc(mf);
").

:- pragma foreign_proc("C#",
    io.read_byte_val(File::in, ByteVal::out, _IO0::di, _IO::uo),
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
    io.putback_char(File::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"{
    MR_MercuryFileStruct mf = File;
    mercury_ungetc(mf, Character);
}").

:- pragma foreign_proc("C#",
    io.putback_byte(File::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"{
    MR_MercuryFileStruct mf = File;
    if (mf.putback != -1) {
        mercury.runtime.Errors.SORRY(
            ""io.putback_byte: max one character of putback"");
    }
    mf.putback = Byte;
}").

:- pragma foreign_proc("Java",
    io.read_char_code(File::in, CharCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    CharCode = File.read_char();
").

:- pragma foreign_proc("Java",
    io.read_byte_val(File::in, ByteVal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    ByteVal = File.read_byte();
").

:- pragma foreign_proc("Java",
    io.putback_char(File::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"
    File.ungetc(Character);
").

:- pragma foreign_proc("Java",
    io.putback_byte(File::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"
    File.ungetc(Byte);
").

/* output predicates - with output to mercury_current_text_output */

:- pragma foreign_proc("C",
    io.write_string(Message::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    mercury_print_string(mercury_current_text_output, Message);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.write_char(Character::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
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
    io.write_int(Val::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    if (ML_fprintf(mercury_current_text_output, ""%ld"", (long) Val) < 0) {
        mercury_output_error(mercury_current_text_output);
    }
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.write_float(Val::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
    MR_sprintf_float(buf, Val);
    if (ML_fprintf(mercury_current_text_output, ""%s"", buf) < 0) {
        mercury_output_error(mercury_current_text_output);
    }
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.write_byte(Byte::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
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
    io.write_bytes(Message::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    mercury_print_binary_string(mercury_current_binary_output, Message);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.flush_output(IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    if (MR_FLUSH(*mercury_current_text_output) < 0) {
        mercury_output_error(mercury_current_text_output);
    }
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.flush_binary_output(IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    if (MR_FLUSH(*mercury_current_binary_output) < 0) {
        mercury_output_error(mercury_current_binary_output);
    }
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
    io.write_string(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_print_string(mercury_current_text_output, Message);
").

:- pragma foreign_proc("C#",
    io.write_char(Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    /* See mercury_output_string() for comments */
    if (mercury_current_text_output.writer == null) {
        mercury_current_text_output.writer =
            new System.IO.StreamWriter(mercury_current_text_output.stream,
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
    io.write_int(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_print_string(mercury_current_text_output, Val.ToString());
").

:- pragma foreign_proc("C#",
    io.write_byte(Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_current_binary_output.stream.WriteByte(
        System.Convert.ToByte(Byte));
").

:- pragma foreign_proc("C#",
    io.write_bytes(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    mercury_print_binary_string(mercury_current_binary_output, Message);
}").

:- pragma foreign_proc("C#",
    io.flush_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_current_text_output.stream.Flush();
").

:- pragma foreign_proc("C#",
    io.flush_binary_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_current_binary_output.stream.Flush();
").

:- pragma foreign_proc("Java",
    io.write_string(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    System.out.print(Message);
").
:- pragma foreign_proc("Java",
    io.write_char(Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    System.out.print(Character);
").
:- pragma foreign_proc("Java",
    io.write_int(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    System.out.print(Val);
").
:- pragma foreign_proc("Java",
    io.write_float(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    System.out.print(Val);
").

:- pragma foreign_proc("Java",
    io.write_byte(Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_current_binary_output.put((byte) Byte);
").

:- pragma foreign_proc("Java",
    io.write_bytes(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    mercury_current_binary_output.write(Message);
}").

:- pragma foreign_proc("Java",
    io.flush_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_current_text_output.flush();
").

:- pragma foreign_proc("Java",
    io.flush_binary_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury_current_binary_output.flush();
").

io.write_float(Float, !IO) :-
    io.write_string(string.float_to_string(Float), !IO).

% Moving about binary streams.

:- pred whence_to_int(io.whence::in, int::out) is det.

whence_to_int(set, 0).
whence_to_int(cur, 1).
whence_to_int(end, 2).

io.seek_binary(Stream, Whence, Offset, IO0, IO) :-
    whence_to_int(Whence, Flag),
    io.seek_binary_2(Stream, Flag, Offset, IO0, IO).

:- pred io.seek_binary_2(io.stream::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.seek_binary_2(Stream::in, Flag::in, Off::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };

    /* XXX should check for failure */
    /* XXX should also check if the stream is seekable */
    if (MR_IS_FILE_STREAM(*Stream)) {
        fseek(MR_file(*Stream), Off, seek_flags[Flag]);
    } else {
        mercury_io_error(Stream, ""io.seek_binary_2: unseekable stream"");
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.binary_stream_offset(Stream::in, Offset::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    /* XXX should check for failure */
    /* XXX should check if the stream is tellable */
    if (MR_IS_FILE_STREAM(*Stream)) {
        Offset = ftell(MR_file(*Stream));
    } else {
        mercury_io_error(Stream,
            ""io.binary_stream_offset: untellable stream"");
    }
    MR_update_io(IO0, IO);
}").

/* output predicates - with output to the specified stream */

:- pragma foreign_proc("C",
    io.write_string(Stream::in, Message::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    mercury_print_string(Stream, Message);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.write_char(Stream::in, Character::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
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
    io.write_int(Stream::in, Val::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"{
    if (ML_fprintf(Stream, ""%ld"", (long) Val) < 0) {
        mercury_output_error(Stream);
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.write_float(Stream::in, Val::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
    MR_sprintf_float(buf, Val);
    if (ML_fprintf(Stream, ""%s"", buf) < 0) {
        mercury_output_error(Stream);
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.write_byte(Stream::in, Byte::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    /* call putc with a strictly non-negative byte-sized integer */
    if (MR_PUTCH(*Stream, (int) ((unsigned char) Byte)) < 0) {
        mercury_output_error(Stream);
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.write_bytes(Stream::in, Message::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    mercury_print_binary_string(Stream, Message);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.flush_output(Stream::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    if (MR_FLUSH(*Stream) < 0) {
        mercury_output_error(Stream);
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.flush_binary_output(Stream::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    if (MR_FLUSH(*Stream) < 0) {
        mercury_output_error(Stream);
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.write_string(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    mercury_print_string(Stream, Message);
}").

:- pragma foreign_proc("C#",
    io.write_char(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
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
    io.write_int(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    mercury_print_string(Stream, Val.ToString());
}").

:- pragma foreign_proc("C#",
    io.write_byte(Stream::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    Stream.stream.WriteByte(System.Convert.ToByte(Byte));
}").

:- pragma foreign_proc("C#",
    io.write_bytes(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    mercury_print_binary_string(Stream, Message);
}").

:- pragma foreign_proc("C#",
    io.flush_output(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    Stream.stream.Flush();
}").

:- pragma foreign_proc("C#",
    io.flush_binary_output(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    Stream.stream.Flush();
}").

:- pragma foreign_proc("Java",
    io.seek_binary_2(Stream::in, Flag::in, Off::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    Stream.seek(Flag, Off);
").

:- pragma foreign_proc("Java",
    io.binary_stream_offset(Stream::in, Offset::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    Offset = Stream.getOffset();
").

:- pragma foreign_proc("Java",
    io.write_string(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream.write(Message);
").

:- pragma foreign_proc("Java",
    io.write_char(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream.put(Character);
").

:- pragma foreign_proc("Java",
    io.write_int(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    Stream.write(java.lang.String.valueOf(Val));
").

:- pragma foreign_proc("Java",
    io.write_float(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    Stream.write(java.lang.String.valueOf(Val));
").

:- pragma foreign_proc("Java",
    io.write_byte(Stream::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream.put(Byte);
").

:- pragma foreign_proc("Java",
    io.write_bytes(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream.write(Message);
").

:- pragma foreign_proc("Java",
    io.flush_output(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream.flush();
").

:- pragma foreign_proc("Java",
    io.flush_binary_output(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream.flush();
").

io.write_float(Stream, Float, !IO) :-
    io.write_string(Stream, string.float_to_string(Float), !IO).

% Stream predicates.

:- pragma export(io.stdin_stream(out, di, uo), "ML_io_stdin_stream").
:- pragma export(io.stdout_stream(out, di, uo), "ML_io_stdout_stream").
:- pragma export(io.stderr_stream(out, di, uo), "ML_io_stderr_stream").

:- pragma foreign_proc("C",
    io.stdin_stream = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Stream = &mercury_stdin;
").

:- pragma foreign_proc("C",
    io.stdin_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = &mercury_stdin;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.stdout_stream = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Stream = &mercury_stdout;
").

:- pragma foreign_proc("C",
    io.stdout_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = &mercury_stdout;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.stderr_stream = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Stream = &mercury_stderr;
").

:- pragma foreign_proc("C",
    io.stderr_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = &mercury_stderr;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.stdin_binary_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = &mercury_stdin_binary;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.stdout_binary_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = &mercury_stdout_binary;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.input_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_text_input;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.output_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_text_output;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.binary_input_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_binary_input;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.binary_output_stream(Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_binary_output;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.get_line_number(LineNum::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = MR_line_number(*mercury_current_text_input);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.get_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = MR_line_number(*Stream);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.set_line_number(LineNum::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MR_line_number(*mercury_current_text_input) = LineNum;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.set_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    MR_line_number(*Stream) = LineNum;
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.get_output_line_number(LineNum::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = MR_line_number(*mercury_current_text_output);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.get_output_line_number(Stream::in, LineNum::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = MR_line_number(*Stream);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.set_output_line_number(LineNum::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MR_line_number(*mercury_current_text_output) = LineNum;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.set_output_line_number(Stream::in, LineNum::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    MR_line_number(*Stream) = LineNum;
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C",
    io.set_input_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_text_input;
    mercury_current_text_input = NewStream;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.set_output_stream(NewStream::in, OutStream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_text_output;
    mercury_current_text_output = NewStream;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.set_binary_input_stream(NewStream::in, OutStream::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_binary_input;
    mercury_current_binary_input = NewStream;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.set_binary_output_stream(NewStream::in, OutStream::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_binary_output;
    mercury_current_binary_output = NewStream;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
    io.stdin_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdin;
").

:- pragma foreign_proc("C#",
    io.stdout_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdout;
").

:- pragma foreign_proc("C#",
    io.stderr_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stderr;
").

:- pragma foreign_proc("C#",
    io.stdin_binary_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdin_binary;
").

:- pragma foreign_proc("C#",
    io.stdout_binary_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdout_binary;
").

:- pragma foreign_proc("C#",
    io.input_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_text_input;
").

:- pragma foreign_proc("C#",
    io.output_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_text_output;
").

:- pragma foreign_proc("C#",
    io.binary_input_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_binary_input;
").

:- pragma foreign_proc("C#",
    io.binary_output_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_binary_output;
").

:- pragma foreign_proc("C#",
    io.get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury_current_text_input.line_number;
").

:- pragma foreign_proc("C#",
    io.get_line_number(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").

:- pragma foreign_proc("C#",
    io.set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury_current_text_input.line_number = LineNum;
").

:- pragma foreign_proc("C#",
    io.set_line_number(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
    io.get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury_current_text_output.line_number;
").

:- pragma foreign_proc("C#",
    io.get_output_line_number(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").

:- pragma foreign_proc("C#",
    io.set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury_current_text_output.line_number = LineNum;
").

:- pragma foreign_proc("C#",
    io.set_output_line_number(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
    io.set_input_stream(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_text_input;
    mercury_current_text_input = NewStream;
").

:- pragma foreign_proc("C#",
    io.set_output_stream(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_text_output;
    mercury_current_text_output = NewStream;
").

:- pragma foreign_proc("C#",
    io.set_binary_input_stream(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_binary_input;
    mercury_current_binary_input = NewStream;
").

:- pragma foreign_proc("C#",
    io.set_binary_output_stream(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_binary_output;
    mercury_current_binary_output = NewStream;
").

:- pragma foreign_proc("Java",
    io.stdin_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdin;
").

:- pragma foreign_proc("Java",
    io.stdout_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdout;
").

:- pragma foreign_proc("Java",
    io.stderr_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stderr;
").

:- pragma foreign_proc("Java",
    io.stdin_binary_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdin_binary;
").

:- pragma foreign_proc("Java",
    io.stdout_binary_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury_stdout_binary;
").

:- pragma foreign_proc("Java",
    io.input_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_text_input;
").

:- pragma foreign_proc("Java",
    io.output_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_text_output;
").

:- pragma foreign_proc("Java",
    io.binary_input_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_binary_input;
").

:- pragma foreign_proc("Java",
    io.binary_output_stream(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury_current_binary_output;
").

:- pragma foreign_proc("Java",
    io.get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury_current_text_input.line_number;
").

:- pragma foreign_proc("Java",
    io.get_line_number(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").

:- pragma foreign_proc("Java",
    io.set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury_current_text_input.line_number = LineNum;
").

:- pragma foreign_proc("Java",
    io.set_line_number(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("Java",
    io.get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury_current_text_output.line_number;
").

:- pragma foreign_proc("Java",
    io.get_output_line_number(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").

:- pragma foreign_proc("Java",
    io.set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury_current_text_output.line_number = LineNum;
").

:- pragma foreign_proc("Java",
    io.set_output_line_number(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

    % io.set_input_stream(NewStream, OldStream, IO0, IO1)
    % Changes the current input stream to the stream specified.
    % Returns the previous stream.

:- pragma foreign_proc("Java",
    io.set_input_stream(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_text_input;
    mercury_current_text_input = NewStream;
").

:- pragma foreign_proc("Java",
    io.set_output_stream(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_text_output;
    mercury_current_text_output = NewStream;
").

:- pragma foreign_proc("Java",
    io.set_binary_input_stream(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_binary_input;
    mercury_current_binary_input = NewStream;
").

:- pragma foreign_proc("Java",
    io.set_binary_output_stream(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury_current_binary_output;
    mercury_current_binary_output = NewStream;
").

% Stream open/close predicates.

    % io.do_open_binary(File, Mode, ResultCode, StreamId, Stream, !IO):
    % io.do_open_text(File, Mode, ResultCode, StreamId, Stream, !IO):
    %
    % Attempts to open a file in the specified mode.
    % The Mode is a string suitable for passing to fopen().
    % Result is 0 for success, -1 for failure.
    % StreamId is a unique integer identifying the open.
    % Both StreamId and Stream are valid only if Result == 0.
    %
:- pred io.do_open_binary(string::in, string::in, int::out, int::out,
    io.input_stream::out, io::di, io::uo) is det.

:- pred io.do_open_text(string::in, string::in, int::out, int::out,
    io.input_stream::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.do_open_text(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = mercury_open(FileName, Mode);
    ResultCode = (Stream != NULL ? 0 : -1);
    StreamId = (Stream != NULL ? ML_next_stream_id++ : -1);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.do_open_binary(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Stream = mercury_open(FileName, Mode);
    ResultCode = (Stream != NULL ? 0 : -1);
    StreamId = (Stream != NULL ? ML_next_stream_id++ : -1);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
    io.do_open_text(FileName::in, Mode::in, ResultCode::out,
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
    io.do_open_binary(FileName::in, Mode::in, ResultCode::out,
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

:- pragma foreign_proc("Java",
    io.do_open_text(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (Mode.charAt(0) == 'r') {
            Stream = new MR_MercuryFileStruct(
                new java.io.FileInputStream(FileName));
        } else if (Mode.charAt(0) == 'w') {
            Stream = new MR_MercuryFileStruct(
                new java.io.FileOutputStream(FileName));
        } else if (Mode.charAt(0) == 'a') {
            Stream = new MR_MercuryFileStruct(
                new java.io.FileOutputStream(FileName, true));
        } else {
            throw new java.lang.RuntimeException(""io.do_open_text: "" +
                ""Invalid open mode"" + "" \\"""" + Mode + ""\\"""");
        }
        StreamId = Stream.id;
        ResultCode = 0;
    } catch (java.lang.Exception e) {
        MR_io_exception = e;
        Stream = null;
        StreamId = -1;
        ResultCode = -1;
    }
").

:- pragma foreign_proc("Java",
    io.do_open_binary(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Stream = new MR_MercuryFileStruct(FileName, Mode.charAt(0));
        StreamId = Stream.id;
        ResultCode = 0;
    } catch (java.lang.Exception e) {
        MR_io_exception = e;
        Stream = null;
        StreamId = -1;
        ResultCode = -1;
    }
").

io.close_input(Stream, !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

io.close_output(Stream, !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

io.close_binary_input(Stream, !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

io.close_binary_output(Stream, !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

:- pred io.close_stream(stream::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.close_stream(Stream::in, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    mercury_close(Stream);
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C#",
    io.close_stream(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    mercury_close(Stream);
").

:- pragma foreign_proc("Java",
    io.close_stream(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    Stream.close();
").

% Miscellaneous predicates.

:- pragma foreign_proc("C",
    io.progname(DefaultProgname::in, PrognameOut::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (MR_progname) {
        MR_make_aligned_string(PrognameOut, MR_progname);
    } else {
        PrognameOut = DefaultProgname;
    }
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.command_line_arguments(Args::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    int i;

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
    io.get_exit_status(ExitStatus::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ExitStatus = mercury_exit_status;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.set_exit_status(ExitStatus::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury_exit_status = ExitStatus;
    MR_update_io(IO0, IO);
").

:- pragma foreign_proc("C",
    io.call_system_code(Command::in, Status::out, Msg::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Status = system(Command);
    if (Status == -1) {
        /*
        ** Return values of 127 or -1 from system() indicate that
        ** the system call failed. Don't return -1, as -1 indicates
        ** that the system call was killed by signal number 1.
        */
        Status = 127;
        ML_maybe_make_err_msg(MR_TRUE, errno,
            ""error invoking system command: "", MR_PROC_LABEL, Msg);
    } else {
        Msg = MR_make_string_const("""");
    }
    MR_update_io(IO0, IO);
").

io.progname(DefaultProgName::in, ProgName::out, IO::di, IO::uo) :-
    % This is a fall-back for back-ends which don't support the C interface.
    ProgName = DefaultProgName.

io.handle_system_command_exit_status(Code0) = Status :-
    Code = io.handle_system_command_exit_code(Code0),
    ( Code = 127 ->
        Status = error(io_error("unknown result code from system command"))
    ; Code < 0 ->
        Status = ok(signalled(-Code))
    ;
        Status = ok(exited(Code))
    ).

    % Interpret the child process exit status returned by system() or wait():
    % return negative for `signalled', non-negative for `exited', or 127
    % for anything else (e.g. an error invoking the command).
    %
:- func io.handle_system_command_exit_code(int) = int.

io.handle_system_command_exit_code(Status0::in) = (Status::out) :-
    % This is a fall-back for back-ends that don't support the C interface.
    ( (Status0 /\ 0xff) \= 0 ->
        /* the process was killed by a signal */
        Status = -(Status0 /\ 0xff)
    ;
        /* the process terminated normally */
        Status = (Status0 /\ 0xff00) >> 8
    ).

:- pragma foreign_proc("C",
    io.handle_system_command_exit_code(Status0::in) = (Status::out),
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
        if ((Status0 & 0xff) != 0) {
            /* the process was killed by a signal */
            Status = -(Status0 & 0xff);
        } else {
            /* the process terminated normally */
            Status = (Status0 & 0xff00) >> 8;
        }
    #endif
").

:- pragma foreign_proc("C#",
    io.command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    string[] arg_vector = System.Environment.GetCommandLineArgs();
    int i = arg_vector.Length;
    Args = mercury.list.mercury_code.ML_empty_list(null);
    // We don't get the 0th argument: it is the executable name.
    while (--i > 0) {
        Args = mercury.list.mercury_code.ML_cons(null, arg_vector[i], Args);
    }
").

:- pragma foreign_proc("C#",
    io.get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ExitStatus = System.Environment.ExitCode;
").

:- pragma foreign_proc("C#",
    io.set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    System.Environment.ExitCode = ExitStatus;
").

:- pragma foreign_proc("C#",
    io.call_system_code(Command::in, Status::out, Msg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    try {
        // XXX This could be better... need to handle embedded spaces
        // in the command name.
        int index = Command.IndexOf("" "");
        string command = Command.Substring(0, index);
        string arguments = Command.Remove(0, index + 1);

        // debugging...
        // System.Console.Out.WriteLine(
        //  ""[command = "" + command + ""]"");
        // System.Console.Out.WriteLine(
        //  ""[arguments = "" + arguments + ""]"");

        System.Diagnostics.Process process =
            System.Diagnostics.Process.Start(command, arguments);
        process.WaitForExit();
        Status = process.ExitCode;
        Msg = """";

        // debugging...
        // System.Console.Out.WriteLine(""[exitcode = "" + Status + ""]"");

    }
    catch (System.Exception e) {
        Status = 127;
        Msg = e.Message;

        // debugging...
        // System.Console.Out.WriteLine(""[message = "" + Msg + ""]"");
    }
").

:- pragma foreign_proc("Java",
    io.progname(_Default::in, PrognameOut::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    PrognameOut = mercury.runtime.JavaInternal.progname;
").

:- pragma foreign_proc("Java",
    io.get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ExitStatus = mercury.runtime.JavaInternal.exit_status;
").

:- pragma foreign_proc("Java",
    io.set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury.runtime.JavaInternal.exit_status = ExitStatus;
").

io.command_line_arguments(Args, IO, IO) :-
    build_command_line_args(0, Args).

:- pred build_command_line_args(int::in, list(string)::out) is det.

build_command_line_args(ArgNumber, Args) :-
    ( command_line_argument(ArgNumber, Arg) ->
        Args = [Arg | MoreArgs],
        build_command_line_args(ArgNumber + 1, MoreArgs)
    ;
        Args = []
    ).

:- pred command_line_argument(int::in, string::out) is semidet.

command_line_argument(_, "") :-
    % XXX This predicate is currently only used by the Java implementation,
    % but to prevent compilation warnings for (eg) the C implementation,
    % some definition needs to be present.
    ( semidet_succeed ->
        error("unexpected call to command_line_argument")
    ;
        semidet_fail
    ).

:- pragma foreign_proc("Java",
    command_line_argument(ArgNum::in, Arg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    String[] arg_vector = mercury.runtime.JavaInternal.args;

    if (ArgNum < arg_vector.length && ArgNum >= 0) {
        Arg = arg_vector[ArgNum];
        succeeded = true;
    } else {
        Arg = null;
        succeeded = false;
    }
").

:- pragma foreign_proc("Java",
    io.call_system_code(Command::in, Status::out, Msg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    try {
        java.lang.Process process = java.lang.Runtime.getRuntime().
            exec(Command);

        StreamPipe stdin = new StreamPipe(mercury_stdin,
            process.getOutputStream());
        StreamPipe stdout = new StreamPipe(process.getInputStream(),
            mercury_stdout);
        StreamPipe stderr = new StreamPipe(process.getErrorStream(),
            mercury_stderr);
        stdin.start();
        stdout.start();
        stderr.start();

        Status  = process.waitFor();
        Msg = null;

        // The StreamPipes are killed off after the Process is finished,
        // so as not to waste CPU cycles with pointless threads.
        stdin.interrupt();
        stdout.interrupt();
        stderr.interrupt();

        if (stdin.exception != null) {
            throw stdin.exception;
        }
        if (stdout.exception != null) {
            throw stdout.exception;
        }
        if (stderr.exception != null) {
            throw stderr.exception;
        }
    } catch (java.lang.Exception e) {
        Status  = 127;
        Msg = e.getMessage();
    }
").

/*---------------------------------------------------------------------------*/

% io.getenv and io.setenv.

:- pragma foreign_decl("C", "
#include <stdlib.h> /* for getenv() and putenv() */
").

:- pragma promise_semipure(io.getenv/2).

:- pragma foreign_proc("C",
    io.getenv(Var::in, Value::out),
    [will_not_call_mercury, tabled_for_io],
"{
    Value = getenv(Var);
    SUCCESS_INDICATOR = (Value != 0);
}").

:- pragma foreign_proc("C#",
    io.getenv(Var::in, Value::out),
    [will_not_call_mercury, tabled_for_io],
"{
    Value = System.Environment.GetEnvironmentVariable(Var);
    SUCCESS_INDICATOR = (Value != null);
}").

:- pragma foreign_proc("Java",
    io.getenv(Var::in, Value::out),
    [will_not_call_mercury, tabled_for_io],
"
    // Note that this approach only works for environmental variables that
    // are recognized by Java and hava a special format.
    // (eg os.name, user.timezone etc)
    // To add custom environmental variables, they must be set at the
    // command line with java's -D option
    // (ie java -DENV_VARIABLE_NAME=$ENV_VARIABLE_NAME ClassName)
    // XXX Perhaps a better approach would be to determine the OS at
    // runtime and then Runtime.exec() the equivalent of 'env'?

    try {
        Value = java.lang.System.getProperty(Var);
        succeeded = (Value != null);
    } catch (java.lang.Exception e) {
        Value = null;
        succeeded = false;
    }
").

io.setenv(Var, Value) :-
    impure io.putenv(Var ++ "=" ++ Value).

    % io.putenv(VarString): If VarString is a string of the form "name=value",
    % sets the environment variable name to the specified value. Fails if
    % the operation does not work. Not supported for .NET. This should only be
    % called from io.setenv.
    %
:- impure pred io.putenv(string::in) is semidet.

:- pragma foreign_proc("C",
    io.putenv(VarAndValue::in),
    [will_not_call_mercury, tabled_for_io],
"
    SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);
").

:- pragma foreign_proc("C#",
    io.putenv(VarAndValue::in),
    [will_not_call_mercury, tabled_for_io],
"
    /*
    ** Unfortunately there is no API in the .NET standard library for setting
    ** environment variables. So we need to use platform-specific methods.
    ** Currently we use the Posix function putenv(), which is also supported
    ** on Windows.
    */
    SUCCESS_INDICATOR = (mercury.runtime.PInvoke._putenv(VarAndValue) == 0);
").

:- pragma foreign_proc("Java",
    io.setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io],
"
    // XXX see io.getenv/2

    try {
        Value = java.lang.System.setProperty(Var, Value);
        succeeded = true;
    } catch (java.lang.Exception e) {
        succeeded = false;
    }
").

:- pragma foreign_proc("Java",
    io.putenv(VarAndValue::in),
    [will_not_call_mercury, tabled_for_io],
"
    // This procedure should never be called, as io.setenv/2 has been
    // implemented directly for Java.
    // This implementation is included only to suppress warnings.

    throw new RuntimeException(
        ""io.putenv/1 not implemented for Java: "" + VarAndValue);
").

%-----------------------------------------------------------------------------%

    % We need to do an explicit check of TMPDIR because not all
    % systems check TMPDIR for us (eg Linux #$%*@&).
io.make_temp(Name, !IO) :-
    Var = ( dir.use_windows_paths -> "TMP" ; "TMPDIR" ),
    io.get_environment_var(Var, Result, !IO),
    (
        Result = yes(Dir)
    ;
        Result = no,
        ( dir.use_windows_paths ->
            Dir = dir.this_directory
        ;
            Dir = "/tmp"
        )
    ),
    io.make_temp(Dir, "mtmp", Name, !IO).

io.make_temp(Dir, Prefix, Name, !IO) :-
    io.do_make_temp(Dir, Prefix, char_to_string(dir.directory_separator),
        Name, Err, Message, !IO),
    ( Err \= 0 ->
        throw_io_error(Message)
    ;
        true
    ).

/*---------------------------------------------------------------------------*/

:- pred io.do_make_temp(string::in, string::in, string::in,
    string::out, int::out, string::out, io::di, io::uo) is det.

% XXX The code for io.make_temp assumes POSIX. It uses the functions open(),
% close(), and getpid() and the macros EEXIST, O_WRONLY, O_CREAT, and O_EXCL.
% We should be using conditional compilation here to avoid these POSIX
% dependencies.

%#include <stdio.h>

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_UNISTD_H
    #include <unistd.h>
#endif
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <fcntl.h>

    #define ML_MAX_TEMPNAME_TRIES   (6 * 4)

    extern long ML_io_tempnam_counter;
").

:- pragma foreign_code("C", "
    long    ML_io_tempnam_counter = 0;
").

:- pragma foreign_proc("C",
    io.do_make_temp(Dir::in, Prefix::in, Sep::in, FileName::out,
        Error::out, ErrorMessage::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    /*
    ** Constructs a temporary name by concatenating Dir, `/', the first 5 chars
    ** of Prefix, three hex digits, '.', and 3 more hex digits. The six digit
    ** hex number is generated by starting with the pid of this process.
    ** Uses `open(..., O_CREATE | O_EXCL, ...)' to create the file, checking
    ** that there was no existing file with that name.
    */
    int     len, err, fd, num_tries;
    char    countstr[256];
    MR_Word filename_word;

    len = strlen(Dir) + 1 + 5 + 3 + 1 + 3 + 1;
    /* Dir + / + Prefix + counter_high + . + counter_low + \\0 */
    MR_offset_incr_hp_atomic_msg(filename_word, 0,
        (len + sizeof(MR_Word)) / sizeof(MR_Word),
        MR_PROC_LABEL, ""string:string/0"");
    FileName = (MR_String) filename_word;
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
            ""error opening temporary file: "", MR_PROC_LABEL, ErrorMessage);
        Error = -1;
    }  else {
        err = close(fd);
        ML_maybe_make_err_msg(err, errno,
            ""error closing temporary file: "", MR_PROC_LABEL, ErrorMessage);
        Error = err;
    }
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.do_make_temp(Dir::in, Prefix::in, _Sep::in, FileName::out,
        Error::out, ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    /*
    ** XXX For some reason this MC++ code below doesn't work.
    ** We get the following error message:
    **   Unhandled Exception: System.TypeInitializationException: The type
    **   initializer for ""remove_file.mercury_code"" threw an exception.
    **   ---> System.IO.FileLoadException: The dll initialization routine
    **   failed for file 'io__cpp_code.dll'.
    ** so instead we use the .NET call and ignore Dir and Prefix.
    **
    ** int result;
    ** char __nogc *dir = static_cast<char*>(
    **  System::Runtime::InteropServices::Marshal::
    **  StringToHGlobalAnsi(Dir).ToPointer());;
    ** char __nogc *prefix = static_cast<char*>(
    **  System::Runtime::InteropServices::Marshal::
    **  StringToHGlobalAnsi(Prefix).ToPointer());;
    ** char tmpFileName[MAX_PATH];
    ** System::String *msg[] = {
    **  S""Unable to create temporary file in "",
    **  Dir,
    **  S"" with prefix "",
    **  Prefix
    ** };
    **
    ** result = GetTempFileName(dir, prefix, 0, tmpFileName);
    **
    ** if (result == 0) {
    **  Error = -1;
    **  FileName = S"""";
    **  ErrorMessage = System::String::Join(S"""", msg);
    ** } else {
    **  Error = 0;
    **  FileName = tmpFileName;
    **  ErrorMessage = S"""";
    ** }
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

% For the Java implementation, io.make_temp/3 is overwritten directly,
% since Java is capable of locating the default temp directory itself.

:- pragma foreign_proc("Java",
    io.do_make_temp(_Dir::in, _Prefix::in, _Sep::in, _FileName::out,
        _Error::out, _ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    // this function should never be called.

    throw new RuntimeException(""io.do_make_temp not implemented"");
").

:- pragma foreign_proc("Java",
    io.make_temp(FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        java.io.File tmpdir = new java.io.File(
            java.lang.System.getProperty(""java.io.tmpdir""));
        FileName = java.io.File.createTempFile(""mtmp"", null, tmpdir).
            getName();
    } catch (java.lang.Exception e) {
        throw new RuntimeException(e.getMessage());
        // This is done instead of just 'throw e' because otherwise
        // the java compiler complains about unreported exceptions.
    }
").

:- pragma foreign_proc("Java",
    io.make_temp(Dir::in, Prefix::in, FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (Prefix.length() < 3) {
            // File.createTempFile() requires a Prefix which is
            // at least 3 characters long.
            Prefix = Prefix + ""tmp"";
        } else if (Prefix.length() > 5) {
            // The documentation for io.make_temp says that we should only use
            // the first five characters of Prefix.
            Prefix = Prefix.substring(0, 5);
        }
        FileName = java.io.File.createTempFile(Prefix, null,
            new java.io.File(Dir)).getName();
    } catch (java.lang.Exception e) {
        throw new RuntimeException(e.getMessage());
        // This is done instead of just 'throw e' because otherwise
        // the java compiler complains about unreported exceptions.
    }
").

/*---------------------------------------------------------------------------*/

:- pragma foreign_decl("C", "

#include <string.h>
#include <errno.h>

/*
** ML_maybe_make_err_msg(was_error, errno, msg, procname, error_msg):
** if `was_error' is true, then append `msg' and `strerror(errno)'
** to give `error_msg'; otherwise, set `error_msg' to "".
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

#define ML_maybe_make_err_msg(was_error, error, msg, procname, error_msg)   \\
    do {                                                                    \\
        char    *errno_msg;                                                 \\
        size_t  total_len;                                                  \\
        MR_Word tmp;                                                        \\
                                                                            \\
        if (was_error) {                                                    \\
            errno_msg = strerror(error);                                    \\
            total_len = strlen(msg) + strlen(errno_msg);                    \\
            MR_offset_incr_hp_atomic_msg(tmp, 0,                            \\
                (total_len + sizeof(MR_Word)) / sizeof(MR_Word),            \\
                procname, ""string.string/0"");                             \\
            (error_msg) = (char *) tmp;                                     \\
            strcpy((error_msg), msg);                                       \\
            strcat((error_msg), errno_msg);                                 \\
        } else {                                                            \\
            /*                                                              \\
            ** We can't just return NULL here, because otherwise mdb        \\
            ** will break when it tries to print the string.                \\
            */                                                              \\
            (error_msg) = MR_make_string_const("""");                       \\
        }                                                                   \\
    } while(0)

/*
** ML_maybe_make_win32_err_msg(was_error, error, msg, procname, error_msg):
** if `was_error' is true, then append `msg' and the string
** returned by the Win32 API function FormatMessage() for the
** last error to give `error_msg'; otherwise, set `error_msg' to "".
** Aborts if MR_WIN32 is not defined.
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
    do {                                                                    \\
        size_t total_len;                                                   \\
        MR_Word tmp;                                                        \\
                                                                            \\
        if (was_error) {                                                    \\
            LPVOID  err_buf;                                                \\
            MR_bool free_err_buf = MR_TRUE;                                 \\
            if (!FormatMessage(                                             \\
                FORMAT_MESSAGE_ALLOCATE_BUFFER                              \\
                | FORMAT_MESSAGE_FROM_SYSTEM                                \\
                | FORMAT_MESSAGE_IGNORE_INSERTS,                            \\
                NULL,                                                       \\
                error,                                                      \\
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),                  \\
                (LPTSTR) &err_buf,                                          \\
                0,                                                          \\
                NULL))                                                      \\
            {                                                               \\
                free_err_buf = MR_FALSE;                                    \\
                err_buf = ""could not retrieve error message"";             \\
            }                                                               \\
            total_len = strlen(msg) + strlen((char *)err_buf);              \\
            MR_incr_hp_atomic_msg(tmp,                                      \\
                (total_len + sizeof(MR_Word)) / sizeof(MR_Word),            \\
                procname, ""string.string/0"");                             \\
            (error_msg) = (char *) tmp;                                     \\
            strcpy((error_msg), msg);                                       \\
            strcat((error_msg), (char *)err_buf);                           \\
            if (free_err_buf) {                                             \\
                LocalFree(err_buf);                                         \\
            }                                                               \\
        } else {                                                            \\
            /*                                                              \\
            ** We can't just return NULL here, because otherwise mdb        \\
            ** will break when it tries to print the string.                \\
            */                                                              \\
            (error_msg) = MR_make_string_const("""");                       \\
        }                                                                   \\
    } while(0)

#else /* !MR_WIN32 */

#define ML_maybe_make_win32_err_msg(was_error, error, msg, procname, error_msg) \\
    MR_fatal_error(""ML_maybe_make_win32_err_msg called on non-Windows platform"")

#endif /* !MR_WIN32 */

").

io.remove_file(FileName, Result, !IO) :-
    io.remove_file_2(FileName, Res, ResString, !IO),
    ( Res \= 0 ->
        Result = error(io_error(ResString))
    ;
        Result = ok
    ).

:- pred io.remove_file_2(string::in, int::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    io.remove_file_2(FileName::in, RetVal::out, RetStr::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    RetVal = remove(FileName);
    ML_maybe_make_err_msg(RetVal != 0, errno, ""remove failed: "",
        MR_PROC_LABEL, RetStr);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.remove_file_2(FileName::in, RetVal::out, RetStr::out,
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

:- pragma foreign_proc("Java",
    io.remove_file_2(FileName::in, RetVal::out, RetStr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        java.io.File file = new java.io.File(FileName);

        if (file.delete()) {
            RetVal = 0;
            RetStr = """";
        } else {
            RetVal = -1;
            RetStr = ""remove_file failed"";
        }
    } catch (java.lang.Exception e) {
        RetVal = -1;
        RetStr = e.getMessage();
    }
").

io.rename_file(OldFileName, NewFileName, Result, IO0, IO) :-
    io.rename_file_2(OldFileName, NewFileName, Res, ResString, IO0, IO),
    ( Res \= 0 ->
        Result = error(io_error(ResString))
    ;
        Result = ok
    ).

:- pred io.rename_file_2(string::in, string::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.rename_file_2(OldFileName::in, NewFileName::in, RetVal::out,
        RetStr::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    RetVal = rename(OldFileName, NewFileName);
    ML_maybe_make_err_msg(RetVal != 0, errno, ""rename failed: "",
        MR_PROC_LABEL, RetStr);
    MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("C#",
    io.rename_file_2(OldFileName::in, NewFileName::in, RetVal::out,
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

:- pragma foreign_proc("Java",
    io.rename_file_2(OldFileName::in, NewFileName::in, RetVal::out,
        RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        java.io.File file = new java.io.File(OldFileName);

        if (file.exists()) {
            if (file.renameTo(new java.io.File(NewFileName))) {
                RetVal = 0;
                RetStr = """";
            } else {
                RetVal = -1;
                RetStr = ""rename_file failed"";
            }
        } else {
            RetVal = -1;
            RetStr = ""rename failed: No such file or directory"";
        }
    } catch (java.lang.Exception e) {
        RetVal = -1;
        RetStr = e.getMessage();
    }
").

io.have_symlinks :- semidet_fail.

:- pragma foreign_proc("C",
    io.have_symlinks,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_HAVE_SYMLINK) && defined(MR_HAVE_READLINK)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

io.make_symlink(FileName, LinkFileName, Result, !IO) :-
    ( io.have_symlinks ->
        io.make_symlink_2(FileName, LinkFileName, Status, !IO),
        ( Status = 0 ->
            io.make_err_msg("io.make_symlink failed: ", Msg, !IO),
            Result = error(make_io_error(Msg))
        ;
            Result = ok
        )
    ;
        Result = error(make_io_error(
            "io.make_symlink not supported on this platform"))
    ).

:- pred io.make_symlink_2(string::in, string::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.make_symlink_2(FileName::in, LinkFileName::in, Status::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_SYMLINK
    Status = (symlink(FileName, LinkFileName) == 0);
#else
    Status = 0;
#endif
    MR_update_io(IO0, IO);
}").

io.read_symlink(FileName, Result, !IO) :-
    ( io.have_symlinks ->
        io.read_symlink_2(FileName, TargetFileName, Status, Error, !IO),
        ( Status = 0 ->
            io.make_err_msg(Error, "io.read_symlink failed: ", Msg, !IO),
            Result = error(make_io_error(Msg))
        ;
            Result = ok(TargetFileName)
        )
    ;
        Result = error(make_io_error(
            "io.read_symlink not supported on this platform"))
    ).

:- pred io.read_symlink_2(string::in, string::out, int::out,
    io.system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.read_symlink_2(FileName::in, TargetFileName::out,
        Status::out, Error::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
#ifdef MR_HAVE_READLINK
  #ifndef PATH_MAX
    #define PATH_MAX 256
  #endif
    int     num_chars;
    char    *buffer2 = NULL;
    int     buffer_size2 = PATH_MAX;
    char    buffer[PATH_MAX + 1];

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
        MR_make_aligned_string_copy(TargetFileName, buffer);
        Status = 1;
    }
#else /* !MR_HAVE_READLINK */
    /*
    ** We can't just return NULL here, because otherwise mdb will break
    ** when it tries to print the string.
    */
    TargetFileName = MR_make_string_const("""");
    Status = 0;
#endif
    MR_update_io(IO0, IO);
}").

% Since io.have_symlinks will fail for Java, these procedures should never be
% called:

:- pragma foreign_proc("Java",
    io.make_symlink_2(_FileName::in, _LinkFileName::in, _Status::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (true) {
        throw new java.lang.RuntimeException(
            ""io.make_symlink_2 not implemented"");
    }
").

:- pragma foreign_proc("Java",
    io.read_symlink_2(_FileName::in, _TargetFileName::out, _Status::out,
        _Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (true) {
        throw new java.lang.RuntimeException(
            ""io.read_symlink_2 not implemented"");
    }
").

/*---------------------------------------------------------------------------*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

io.error_message(Error) = Msg :-
    io.error_message(Error, Msg).
