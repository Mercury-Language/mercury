%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
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
% We implement a purely logical I/O system using non-logical I/O primitives of
% the underlying system.  We ensure referential transparency by passing around
% a ``state-of-the-world'' argument using unique modes. The compiler will check
% that the state of the world argument is properly single-threaded, and will
% also ensure that you don't attempt to backtrack over any I/O.
%
% Attempting any operation on a stream which has already been closed results
% in undefined behaviour.
%
% In multithreaded programs, each thread in the program has its own set of
% "current" input and output streams. At the time it is created, a child
% thread inherits the current streams from its parent. Predicates that
% change which stream is current affect only the calling thread.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io.
:- interface.

:- import_module bitmap.
:- import_module bool.
:- import_module char.
:- import_module deconstruct.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module stream.
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

    % Alternative names for the above.
    %
:- type io.text_input_stream == io.input_stream.
:- type io.text_output_stream == io.output_stream.

    % Opaque handles for binary I/O streams.
    %
:- type io.binary_input_stream.
:- type io.binary_output_stream.

    % A unique identifier for an I/O stream.
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
    % a partial result when an error occurs.
    %
:- type io.maybe_partial_res(T)
    --->    ok(T)
    ;       error(T, io.error).

:- inst io.maybe_partial_res(T)
    --->    ok(T)
    ;       error(T, ground).

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
    %
:- type io.poly_type == string.poly_type.

    % io.whence denotes the base for a seek operation.
    %   set - seek relative to the start of the file
    %   cur - seek relative to the current position in the file
    %   end - seek relative to the end of the file.
    %
:- type io.whence
    --->    set
    ;       cur
    ;       end.

%-----------------------------------------------------------------------------%
%
% Text input predicates
%

    % Reads a character (code point) from the current input stream.
    %
:- pred io.read_char(io.result(char)::out, io::di, io::uo) is det.

    % Reads a whitespace delimited word from the current input stream.
    %
:- pred io.read_word(io.result(list(char))::out, io::di, io::uo) is det.

    % Reads a line from the current input stream, returns the result
    % as a list of characters (code points).
    %
:- pred io.read_line(io.result(list(char))::out, io::di, io::uo) is det.

    % Reads a line from the current input stream, returns the result
    % as a string. See the documentation for `string.line' for the
    % definition of a line.
    %
:- pred io.read_line_as_string(io.result(string)::out, io::di, io::uo) is det.

    % Reads all the characters (code points) from the current input stream
    % until eof or error.
    %
:- pred io.read_file(io.maybe_partial_res(list(char))::out, io::di, io::uo)
    is det.

    % Reads all the characters (code points) from the current input stream
    % until eof or error. Returns the result as a string rather than
    % as a list of char.
    %
    % Returns an error if the file contains a null character, because
    % null characters are not allowed in Mercury strings.
    %
:- pred io.read_file_as_string(io.maybe_partial_res(string)::out,
    io::di, io::uo) is det.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl(pred(char, T, T), T, io.maybe_partial_res(T),
    io, io).
:- mode io.input_stream_foldl((pred(in, in, out) is det), in, out,
    di, uo) is det.
:- mode io.input_stream_foldl((pred(in, in, out) is cc_multi), in, out,
    di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl_io(pred(char, io, io), io.res, io, io).
:- mode io.input_stream_foldl_io((pred(in, di, uo) is det), out, di, uo)
    is det.
:- mode io.input_stream_foldl_io((pred(in, di, uo) is cc_multi), out, di, uo)
    is cc_multi.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl2_io(pred(char, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl2_io((pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl2_io((pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error, or the closure returns `no' as
    % its second argument.
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

    % Un-reads a character (code point) from the current input stream.
    % You can put back as many characters as you like.
    % You can even put back something that you didn't actually read.
    % Note: `io.putback_char' uses the C library function ungetc().
    % On some systems only one byte of pushback is guaranteed.
    % `io.putback_char' will throw an io.error exception if ungetc() fails.
    %
:- pred io.putback_char(char::in, io::di, io::uo) is det.

    % Reads a character (code point) from specified stream.
    %
:- pred io.read_char(io.input_stream::in, io.result(char)::out,
    io::di, io::uo) is det.

    % Reads a character (code point) from the specified stream.
    % This interface avoids memory allocation when there is no error.
    %
:- pred io.read_char_unboxed(io.input_stream::in, io.result::out, char::out,
    io::di, io::uo) is det.

    % Reads a whitespace delimited word from specified stream.
    %
:- pred io.read_word(io.input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Reads a line from specified stream, returning the result
    % as a list of characters (code point).
    %
:- pred io.read_line(io.input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Reads a line from specified stream, returning the
    % result as a string. See the documentation for `string.line' for
    % the definition of a line.
    %
:- pred io.read_line_as_string(io.input_stream::in, io.result(string)::out,
    io::di, io::uo) is det.

    % Reads all the characters (code points) from the given input stream until
    % eof or error.
    %
:- pred io.read_file(io.input_stream::in,
    io.maybe_partial_res(list(char))::out, io::di, io::uo) is det.

    % Reads all the characters from the given input stream until eof or error.
    % Returns the result as a string rather than as a list of char.
    %
    % Returns an error if the file contains a null character, because
    % null characters are not allowed in Mercury strings.
    %
:- pred io.read_file_as_string(io.input_stream::in,
    io.maybe_partial_res(string)::out, io::di, io::uo) is det.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl(io.input_stream, pred(char, T, T),
    T, io.maybe_partial_res(T), io, io).
:- mode io.input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode io.input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred io.input_stream_foldl_io(io.input_stream, pred(char, io, io),
    io.res, io, io).
:- mode io.input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode io.input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from
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

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error, or the closure returns `no' as
    % its second argument.
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
    % On some systems only one byte of pushback is guaranteed.
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
:- pred io.read(io.input_stream::in, io.read_result(T)::out,
    io::di, io::uo) is det.

    % The type `posn' represents a position within a string.
    %
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
    % io.print/4 writes its second argument to the output stream specified in
    % its first argument.  In all cases, the argument to output can be of any
    % type.  It is output in a format that is intended to be human readable.
    %
    % If the argument is just a single string or character, it will be printed
    % out exactly as is (unquoted).  If the argument is of type integer (i.e.
    % an arbitrary precision integer), then its decimal representation will be
    % printed.  If the argument is of type univ, then it will print out the
    % value stored in the univ, but not the type.
    %
    % io.print/5 is the same as io.print/4 except that it allows the caller to
    % specify how non-canonical types should be handled. io.print/3 and
    % io.print/4 implicitly specify `canonicalize' as the method for handling
    % non-canonical types.  This means that for higher-order types, or types
    % with user-defined equality axioms, or types defined using the foreign
    % language interface (i.e. pragma foreign_type), the text output will only
    % describe the type that is being printed, not the value.
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

:- pred io.print(io.output_stream, deconstruct.noncanon_handling, T, io, io).
:- mode io.print(in, in(do_not_allow), in, di, uo) is det.
:- mode io.print(in, in(canonicalize), in, di, uo) is det.
:- mode io.print(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.print(in, in, in, di, uo) is cc_multi.

:- pred io.print_cc(T::in, io::di, io::uo) is cc_multi.

    % io.print_line calls io.print and then writes a newline character.
    %
:- pred io.print_line(T::in, io::di, io::uo) is det.

:- pred io.print_line(io.output_stream::in, T::in, io::di, io::uo) is det.

:- pred io.print_line(io.output_stream, deconstruct.noncanon_handling, T, io, io).
:- mode io.print_line(in, in(do_not_allow), in, di, uo) is det.
:- mode io.print_line(in, in(canonicalize), in, di, uo) is det.
:- mode io.print_line(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.print_line(in, in, in, di, uo) is cc_multi.

:- pred io.print_line_cc(T::in, io::di, io::uo) is cc_multi.

    % io.write/3 writes its argument to the current output stream.
    % io.write/4 writes its second argument to the output stream specified
    % in its first argument. In all cases, the argument to output may be
    % of any type. The argument is written in a format that is intended to
    % be valid Mercury syntax whenever possible.
    %
    % Strings and characters are always printed out in quotes, using backslash
    % escapes if necessary.  For higher-order types, or for types defined using
    % the foreign language interface (pragma foreign_type), the text output
    % will only describe the type that is being printed, not the value, and the
    % result may not be parsable by `io.read'.  For the types containing
    % existential quantifiers, the type `type_desc' and closure types, the
    % result may not be parsable by `io.read', either.  But in all other cases
    % the format used is standard Mercury syntax, and if you append a period
    % and newline (".\n"), then the results can be read in again using
    % `io.read'.
    %
    % io.write/5 is the same as io.write/4 except that it allows the caller
    % to specify how non-canonical types should be handled.  io.write_cc/3
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

    % io.write_line calls io.write and then writes a newline character.
    %
:- pred io.write_line(T::in, io::di, io::uo) is det.

:- pred io.write_line(io.output_stream::in, T::in, io::di, io::uo) is det.

:- pred io.write_line(io.output_stream, deconstruct.noncanon_handling, T, io, io).
:- mode io.write_line(in, in(do_not_allow), in, di, uo) is det.
:- mode io.write_line(in, in(canonicalize), in, di, uo) is det.
:- mode io.write_line(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode io.write_line(in, in, in, di, uo) is cc_multi.

:- pred io.write_line_cc(T::in, io::di, io::uo) is cc_multi.

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
% Input text stream predicates
%

    % io.see(File, Result, !IO).
    % Attempts to open a file for input, and if successful,
    % sets the current input stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrorCode)'.
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

    % Closes an open input stream.
    % Throw an io.error exception if an I/O error occurs.
    %
:- pred io.close_input(io.input_stream::in, io::di, io::uo) is det.

    % Retrieves the current input stream.
    % Does not modify the I/O state.
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

    % Retrieves the standard input stream.
    % Does not modify the I/O state.
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
% Output text stream predicates
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

    % Retrieves the current output stream.
    % Does not modify the I/O state.
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

    % Retrieves the standard output stream.
    % Does not modify the I/O state.
    %
:- pred io.stdout_stream(io.output_stream::out, io::di, io::uo) is det.

    % Retrieves the standard error stream.
    %
:- func io.stderr_stream = io.output_stream.

    % Retrieves the standard error stream.
    % Does not modify the I/O state.
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
% Binary input predicates
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

    % Fill a bitmap from the current binary input stream.
    % Returns the number of bytes read.
    % On end-of-file, the number of bytes read will be less than the size
    % of the bitmap, and the result will be `ok'.
    %
:- pred io.read_bitmap(bitmap::bitmap_di, bitmap::bitmap_uo,
    int::out, io.res::out, io::di, io::uo) is det.

    % Fill a bitmap from the specified binary input stream.
    % Returns the number of bytes read.
    % On end-of-file, the number of bytes read will be less than the size
    % of the bitmap, and the result will be `ok'.
    %
:- pred io.read_bitmap(io.binary_input_stream::in,
    bitmap::bitmap_di, bitmap::bitmap_uo, int::out, io.res::out,
    io::di, io::uo) is det.

    % io.read_bitmap(StartByte, NumBytes, !Bitmap, BytesRead, Result, !IO)
    %
    % Read NumBytes bytes into a bitmap starting at StartByte
    % from the current binary input stream.
    % Returns the number of bytes read.
    % On end-of-file, the number of bytes read will be less than NumBytes,
    % and the result will be `ok'.
    %
:- pred io.read_bitmap(byte_index::in, num_bytes::in,
    bitmap::bitmap_di, bitmap::bitmap_uo, num_bytes::out,
    io.res::out, io::di, io::uo) is det.

    % io.read_bitmap(Stream, !Bitmap, StartByte, NumBytes,
    %       BytesRead, Result, !IO)
    %
    % Read NumBytes bytes into a bitmap starting at StartByte
    % from the specified binary input stream.
    % Returns the number of bytes read.
    % On end-of-file, the number of bytes read will be less than NumBytes,
    % and the result will be `ok'.
    %
:- pred io.read_bitmap(io.binary_input_stream::in,
    byte_index::in, num_bytes::in, bitmap::bitmap_di, bitmap::bitmap_uo,
    num_bytes::out, io.res::out, io::di, io::uo) is det.

    % Reads all the bytes from the current binary input stream
    % until eof or error into a bitmap.
    %
:- pred io.read_binary_file_as_bitmap(io.res(bitmap)::out,
    io::di, io::uo) is det.

    % Reads all the bytes from the given binary input stream into a bitmap
    % until eof or error.
    %
:- pred io.read_binary_file_as_bitmap(io.binary_input_stream::in,
    io.res(bitmap)::out, io::di, io::uo) is det.

    % Reads all the bytes from the current binary input stream
    % until eof or error.
    %
:- pred io.read_binary_file(io.result(list(int))::out, io::di, io::uo) is det.

    % Reads all the bytes from the given binary input stream until
    % eof or error.
    %
:- pred io.read_binary_file(io.binary_input_stream::in,
    io.result(list(int))::out, io::di, io::uo) is det.

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
    % Pushing back a byte decrements the file position by one, except when
    % the file position is already zero, in which case the new file position
    % is unspecified.
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
    % Pushing back a byte decrements the file position by one, except when
    % the file position is already zero, in which case the new file position
    % is unspecified.
    %
:- pred io.putback_byte(io.binary_input_stream::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Binary output predicates
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

    % Write a bitmap to the current binary output stream.
    % The bitmap must not contain a partial final byte.
    %
:- pred io.write_bitmap(bitmap, io, io).
%:- mode io.write_bitmap(bitmap_ui, di, uo) is det.
:- mode io.write_bitmap(in, di, uo) is det.

    % io.write_bitmap(BM, StartByte, NumBytes, !IO).
    % Write part of a bitmap to the current binary output stream.
    %
:- pred io.write_bitmap(bitmap, int, int, io, io).
%:- mode io.write_bitmap(bitmap_ui, in, in, di, uo) is det.
:- mode io.write_bitmap(in, in, in, di, uo) is det.

    % Write a bitmap to the specified binary output stream.
    % The bitmap must not contain a partial final byte.
    %
:- pred io.write_bitmap(io.binary_output_stream, bitmap, io, io).
%:- mode io.write_bitmap(in, bitmap_ui, di, uo) is det.
:- mode io.write_bitmap(in, in, di, uo) is det.

    % io.write_bitmap(Stream, BM, StartByte, NumBytes, !IO).
    % Write part of a bitmap to the specified binary output stream.
    %
:- pred io.write_bitmap(io.binary_output_stream, bitmap, int, int, io, io).
%:- mode io.write_bitmap(in, bitmap_ui, in, in, di, uo) is det.
:- mode io.write_bitmap(in, in, in, in, di, uo) is det.

    % Flush the output buffer of the current binary output stream.
    %
:- pred io.flush_binary_output(io::di, io::uo) is det.

    % Flush the output buffer of the specified binary output stream.
    %
:- pred io.flush_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary input stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
    % A successful seek undoes any effects of io.putback_byte on the stream.
    %
:- pred io.seek_binary_input(io.binary_input_stream::in, io.whence::in,
    int::in, io::di, io::uo) is det.

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary output stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
:- pred io.seek_binary_output(io.binary_output_stream::in, io.whence::in,
    int::in, io::di, io::uo) is det.

    % Returns the offset (in bytes) into the specified binary input stream.
    %
:- pred io.binary_input_stream_offset(io.binary_input_stream::in, int::out,
    io::di, io::uo) is det.

    % Returns the offset (in bytes) into the specified binary output stream.
    %
:- pred io.binary_output_stream_offset(io.binary_output_stream::in, int::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Binary input stream predicates
%

    % Attempts to open a file for binary input, and if successful sets
    % the current binary input stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrorCode)'.
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
    % Does not modify the I/O state.
    %
:- pred io.binary_input_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

    % Changes the current input stream to the stream specified.
    % Returns the previous stream.
    %
:- pred io.set_binary_input_stream(io.binary_input_stream::in,
    io.binary_input_stream::out, io::di, io::uo) is det.

    % Retrieves the standard binary input stream.
    % Does not modify the I/O state.
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
% Binary output stream predicates
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
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred io.close_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

    % Retrieves the current binary output stream.
    % Does not modify the I/O state.
    %
:- pred io.binary_output_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

    % Retrieves the standard binary output stream.
    % Does not modify the I/O state.
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
% Global state predicates
%

    % io.progname(DefaultProgname, Progname).
    %
    % Returns the name that the program was invoked with, if available,
    % or DefaultProgname if the name is not available.
    % Does not modify the I/O state.
    %
:- pred io.progname(string::in, string::out, io::di, io::uo) is det.

    % io.progname_base(DefaultProgname, Progname).
    %
    % Like `io.progname', except that it strips off any path name
    % preceding the program name.  Useful for error messages.
    %
:- pred io.progname_base(string::in, string::out, io::di, io::uo) is det.

    % Returns the arguments that the program was invoked with,
    % if available, otherwise an empty list. Does not modify the I/O state.
    %
:- pred io.command_line_arguments(list(string)::out, io::di, io::uo) is det.

    % The I/O state contains an integer used to record the program's exit
    % status. When the program finishes, it will return this exit status
    % to the operating system. The following predicates can be used to get
    % and set the exit status.
    %
:- pred io.get_exit_status(int::out, io::di, io::uo) is det.
:- pred io.set_exit_status(int::in, io::di, io::uo) is det.

    % The I/O state includes a `globals' field which is not used by the
    % standard library, but can be used by the application. The globals field
    % is of type `univ' so that the application can store any data it wants
    % there.  The following predicates can be used to access this global state.
    %
    % Does not modify the I/O state.
    %
:- pred io.get_globals(univ::out, io::di, io::uo) is det.
:- pred io.set_globals(univ::in, io::di, io::uo) is det.

    % io.update_globals(UpdatePred, !IO).
    % Update the `globals' field in the I/O state based upon its current
    % value.  This is equivalent to the following:
    %
    %   io.get_globals(Globals0, !IO),
    %   UpdatePred(Globals0, Globals),
    %   io.set_globals(Globals, !IO)
    %
    % In parallel grades calls to io.update_globals/3 are atomic.
    % If `UpdatePred' throws an exception then the `globals' field is
    % left unchanged.
    %
:- pred io.update_globals(pred(univ, univ)::in(pred(in, out) is det),
    io::di, io::uo) is det.

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
    % Note: this predicate is not supported on Java.
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

    % io.make_temp(Dir, Prefix, Name, !IO) creates an empty file whose
    % name is different to the name of any existing file. The file will reside
    % in the directory specified by `Dir' and will have a prefix using up to
    % the first 5 characters of `Prefix'. Name is bound to the name of the
    % file. It is the responsibility of the program to delete the file
    % when it is no longer needed.
    %
:- pred io.make_temp(string::in, string::in, string::out, io::di, io::uo)
    is det.

    % io.remove_file(FileName, Result, !IO) attempts to remove the file
    % `FileName', binding Result to ok/0 if it succeeds, or error/1 if it
    % fails. If `FileName' names a file that is currently open, the behaviour
    % is implementation-dependent.
    %
:- pred io.remove_file(string::in, io.res::out, io::di, io::uo) is det.

    % io.remove_file_recursively(FileName, Result, !IO) attempts to remove
    % the file `FileName', binding Result to ok/0 if it succeeds, or error/1
    % if it fails. If `FileName' names a file that is currently open, the
    % behaviour is implementation-dependent.
    %
    % Unlike `io.remove_file', this predicate will attempt to remove non-empty
    % directories (recursively). If it fails, some of the directory elements
    % may already have been removed.
    %
:- pred remove_file_recursively(string::in, io.res::out, io::di, io::uo)
    is det.

    % io.rename_file(OldFileName, NewFileName, Result, !IO).
    %
    % Attempts to rename the file `OldFileName' as `NewFileName', binding
    % Result to ok/0 if it succeeds, or error/1 if it fails. If `OldFileName'
    % names a file that is currently open, the behaviour is
    % implementation-dependent. If `NewFileName' names a file that already
    % exists the behaviour is also implementation-dependent; on some systems,
    % the file previously named `NewFileName' will be deleted and replaced
    % with the file previously named `OldFileName'.
    %
:- pred io.rename_file(string::in, string::in, io.res::out, io::di, io::uo)
    is det.

    % Succeeds if this platform can read and create symbolic links.
    %
:- pred io.have_symlinks is semidet.

    % io.make_symlink(FileName, LinkFileName, Result, !IO).
    %
    % Attempts to make `LinkFileName' be a symbolic link to `FileName'.
    % If `FileName' is a relative path, it is interpreted relative
    % to the directory containing `LinkFileName'.
    %
:- pred io.make_symlink(string::in, string::in, io.res::out, io::di, io::uo)
    is det.

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
    % XXX On the Erlang backend, or on Windows with some compilers, `execute'
    % access is not checked.
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
% Memory management predicates
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
%
% Instances of the stream typeclass
%

:- instance stream.error(io.error).

:- instance stream.stream(io.output_stream, io).
:- instance stream.output(io.output_stream, io).
:- instance stream.writer(io.output_stream, char,   io).
:- instance stream.writer(io.output_stream, float,  io).
:- instance stream.writer(io.output_stream, int,    io).
:- instance stream.writer(io.output_stream, string, io).
:- instance stream.writer(io.output_stream, univ,   io).
:- instance stream.line_oriented(io.output_stream, io).

:- instance stream.stream(io.input_stream, io).
:- instance stream.input(io.input_stream, io).
:- instance stream.reader(io.input_stream, char, io, io.error).
:- instance stream.reader(io.input_stream, line, io, io.error).
:- instance stream.reader(io.input_stream, text_file, io, io.error).

:- instance stream.line_oriented(io.input_stream, io).
:- instance stream.putback(io.input_stream, char, io, io.error).

:- instance stream.stream(io.binary_output_stream, io).
:- instance stream.output(io.binary_output_stream, io).
:- instance stream.writer(io.binary_output_stream, byte, io).
:- instance stream.writer(io.binary_output_stream, bitmap.slice, io).
:- instance stream.seekable(io.binary_output_stream, io).

:- instance stream.stream(io.binary_input_stream,  io).
:- instance stream.input(io.binary_input_stream, io).
:- instance stream.reader(io.binary_input_stream, int, io, io.error).
:- instance stream.bulk_reader(io.binary_input_stream, int,
        bitmap, io, io.error).
:- instance stream.putback(io.binary_input_stream, int, io, io.error).
:- instance stream.seekable(io.binary_input_stream, io).

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
:- pragma foreign_type("C#", io.system_error, "System.Exception").
:- pragma foreign_type(java, io.system_error, "java.lang.Exception").
:- pragma foreign_type(erlang, io.system_error, "").

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
:- impure pred io.get_stream_db_with_locking(io.stream_db::out) is det.

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

    % If the univ contains an I/O stream, return information about that
    % stream, otherwise fail.
:- func get_io_stream_info(io.stream_db, T) = maybe_stream_info is semidet.

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
:- import_module dir.
:- import_module enum.
:- import_module exception.
:- import_module int.
:- import_module map.
:- import_module parser.
:- import_module require.
:- import_module stream.string_writer.
:- import_module term.
:- import_module term_io.
:- import_module type_desc.

:- use_module rtti_implementation.
:- use_module table_builtin.

%-----------------------------------------------------------------------------%

:- pragma foreign_import_module("C", time).   % For ML_construct_time_t.
:- pragma foreign_import_module("C", string).

    % Values of type `io.state' are never really used:
    % instead we store data in global variables.
    % The reason this is defined as a foreign type is to prevent attempts
    % to deconstruct values of the type.
:- type io.state.
:- pragma foreign_type("C", io.state, "MR_Word", [can_pass_as_mercury_type]).
:- pragma foreign_type("IL", io.state, "int32", [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", io.state, "int", [can_pass_as_mercury_type]).
:- pragma foreign_type("Java", io.state, "java.lang.Object",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("Erlang", io.state, "", [can_pass_as_mercury_type]).

:- pragma foreign_decl("C", "
    extern MR_Word      ML_io_stream_db;
    extern MR_Word      ML_io_user_globals;

    extern int          ML_next_stream_id;
    #if 0
      extern MR_Word    ML_io_ops_table;
    #endif

    #ifdef MR_THREAD_SAFE
        extern MercuryLock ML_io_stream_db_lock;
        extern MercuryLock ML_io_user_globals_lock;
        extern MercuryLock ML_io_next_stream_id_lock;
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

    #ifdef MR_THREAD_SAFE
        MercuryLock ML_io_stream_db_lock;
        MercuryLock ML_io_user_globals_lock;
        MercuryLock ML_io_next_stream_id_lock;
    #endif
").

:- pragma foreign_code("C#", "
    // The ML_ prefixes here are not really needed,
    // since the C# code all gets generated inside a class,
    // but we keep them for consistency with the C code.

#if MR_HIGHLEVEL_DATA
    static tree234.Tree234_2            ML_io_stream_db
                                            = new tree234.Tree234_2.Empty_0();
    static univ.Univ_0                  ML_io_user_globals;
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
    static readonly ML_line_ending_kind ML_default_line_ending =
        ML_line_ending_kind.ML_OS_line_ending;

    // Assume UTF-8 encoding on files.  When writing a file, don't emit
    // a byte order mark.
    static readonly System.Text.Encoding text_encoding =
        new System.Text.UTF8Encoding(false);
").

:- pragma foreign_code("Java",
"
    public static tree234.Tree234_2<Integer, Stream_info_0> ML_io_stream_db
        = new tree234.Tree234_2.Empty_0<Integer, Stream_info_0>();
    public static univ.Univ_0 ML_io_user_globals = null;
").

:- type io.stream_putback ==  map(io.stream_id, list(char)).

:- type io.input_stream  ---> input_stream(io.stream).
:- type io.output_stream ---> output_stream(io.stream).

:- type io.binary_input_stream ---> binary_input_stream(io.stream).
:- type io.binary_output_stream ---> binary_output_stream(io.stream).

:- type io.stream
    --->    stream(c_pointer).
:- pragma foreign_type("C", io.stream, "MercuryFilePtr",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("IL", io.stream,
    "class [mercury]mercury.io__csharp_code.MR_MercuryFileStruct").
:- pragma foreign_type("C#", io.stream, "io.MR_MercuryFileStruct").
:- pragma foreign_type("Java", io.stream, "io.MR_MercuryFileStruct").
:- pragma foreign_type("Erlang", io.stream, "").

    % A unique identifier for an I/O stream.
    %
:- type io.stream_id == int.

:- func io.get_stream_id(io.stream) = io.stream_id.

    % This inter-language stuff is tricky.
    % We communicate via ints rather than via io.result_codes because
    % we don't want the C/Java/etc code to depend on how Mercury stores
    % its discriminated union data types.

    % Reads a character (code point) from specified stream, and returns the
    % numerical value for that character (as from char.to_int). This may
    % involve converting external character encodings into Mercury's internal
    % character representation and (for text streams) converting OS line
    % indicators, e.g. CR-LF for Windows, to '\n' characters.
    % Returns -1 if at EOF, -2 if an error occurs.
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
%
% Input predicates
%

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

:- pragma inline(io.read_char_unboxed/5).

io.read_char_unboxed(Stream, Result, Char, !IO) :-
    io.read_char_code(Stream, Code, !IO),
    ( Code = -1 ->
        Result = eof,
        Char = char.det_from_int(0)
    ; char.to_int(Char0, Code) ->
        Result = ok,
        Char = Char0
    ;
        io.make_err_msg("read failed: ", Msg, !IO),
        Result = error(io_error(Msg)),
        Char = char.det_from_int(0)
    ).

% We want to inline these, to allow deforestation.
:- pragma inline(io.read_byte/3).
:- pragma inline(io.read_byte/4).

io.read_byte(Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_byte(Stream, Result, !IO).

io.read_byte(binary_input_stream(Stream), Result, !IO) :-
    io.read_byte_val(input_stream(Stream), Code, !IO),
    ( Code >= 0 ->
        Result = ok(Code)
    ; Code = -1 ->
        Result = eof
    ;
        io.make_err_msg("read failed: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.read_bitmap(!Bitmap, BytesRead, Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_bitmap(Stream, !Bitmap, BytesRead, Result, !IO).

io.read_bitmap(StartByte, NumBytes, !Bitmap, BytesRead, Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_bitmap(Stream, StartByte, NumBytes, !Bitmap,
        BytesRead, Result, !IO).

io.read_bitmap(Stream, !Bitmap, BytesRead, Result, !IO) :-
    ( NumBytes = !.Bitmap ^ num_bytes ->
        io.read_bitmap(Stream, 0, NumBytes, !Bitmap, BytesRead, Result, !IO)
    ;
        error("io.read_bitmap: bitmap contains partial final byte")
    ).

io.read_bitmap(binary_input_stream(Stream), Start, NumBytes, !Bitmap,
        BytesRead, Result, !IO) :-
    (
        NumBytes > 0,
        byte_in_range(!.Bitmap, Start),
        byte_in_range(!.Bitmap, Start + NumBytes - 1)
    ->
        io.do_read_bitmap(Stream, Start, NumBytes,
            !Bitmap, 0, BytesRead, !IO),
        io.ferror(Stream, ErrInt, ErrMsg, !IO),
        ( ErrInt = 0 ->
            Result = ok
        ;
            Result = error(io_error(ErrMsg))
        )
    ;
        NumBytes = 0,
        byte_in_range(!.Bitmap, Start)
    ->
        Result = ok,
        BytesRead = 0
    ;
        bitmap.throw_bounds_error(!.Bitmap, "io.read_bitmap",
                Start * bits_per_byte, NumBytes * bits_per_byte)
    ).

:- pred io.do_read_bitmap(io.stream::in, byte_index::in, num_bytes::in,
    bitmap::bitmap_di, bitmap::bitmap_uo, num_bytes::in, num_bytes::out,
    io::di, io::uo) is det.
:- pragma promise_pure(io.do_read_bitmap/9).

    % Default implementation for C# and Java.
io.do_read_bitmap(Stream, Start, NumBytes, !Bitmap, !BytesRead, !IO) :-
    ( NumBytes > 0 ->
        io.read_byte(binary_input_stream(Stream), ByteResult, !IO),
        (
            ByteResult = ok(Byte),
            !:Bitmap = !.Bitmap ^ unsafe_byte(Start) := Byte,
            !:BytesRead = !.BytesRead + 1,
            io.do_read_bitmap(Stream, Start + 1, NumBytes - 1,
                !Bitmap, !BytesRead, !IO)
        ;
            ByteResult = eof
        ;
            ByteResult = error(_)
        )
    ;
        true
    ).
:- pragma foreign_proc("C",
    io.do_read_bitmap(Stream::in, StartByte::in, NumBytes::in,
        Bitmap0::bitmap_di, Bitmap::bitmap_uo, BytesRead0::in, BytesRead::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Bitmap = Bitmap0,
    BytesRead = BytesRead0 +
                    MR_READ(*Stream, Bitmap->elements + StartByte, NumBytes);
").

io.read_binary_file_as_bitmap(Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_binary_file_as_bitmap(Stream, Result, !IO).

io.read_binary_file_as_bitmap(Stream, Result, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    io.binary_input_stream_file_size(Stream, FileSize, !IO),
    ( FileSize >= 0 ->
        some [!BM] (
            !:BM = bitmap.init(FileSize * bits_per_byte),
            io.read_bitmap(Stream, 0, FileSize,
                !BM, BytesRead, ReadResult, !IO),
            (
                ReadResult = ok,
                ( BytesRead = FileSize ->
                    Result = ok(!.BM)
                ;
                    Result = error(io_error(
                        "io.read_binary_file_as_bitmap: incorrect file size"))
                )
            ;
                ReadResult = error(Msg),
                Result = error(Msg)
            )
        )
    ;
        BufferSize = 4000,
        io.read_binary_file_as_bitmap_2(Stream, BufferSize,
            Res, [], RevBitmaps, !IO),
        (
            Res = ok,
            Result = ok(bitmap.append_list(reverse(RevBitmaps)))
        ;
            Res = error(Msg),
            Result = error(Msg)
        )
    ).

:- pred io.read_binary_file_as_bitmap_2(io.binary_input_stream::in,
    num_bytes::in, io.res::out, list(bitmap)::in, list(bitmap)::out,
    io::di, io::uo) is det.

io.read_binary_file_as_bitmap_2(Stream, BufferSize, Res, !BMs, !IO) :-
    some [!BM] (
        !:BM = bitmap.init(BufferSize * bits_per_byte),
        io.read_bitmap(0, BufferSize, !BM, NumBytesRead, ReadRes, !IO),
        (
            ReadRes = ok,
            ( NumBytesRead < BufferSize ->
                !:BM = bitmap.shrink_without_copying(!.BM,
                        NumBytesRead * bits_per_byte),
                !:BMs = [!.BM | !.BMs],
                Res = ok
            ;
                !:BMs = [!.BM | !.BMs],

                % Double the buffer size each time.
                %
                io.read_binary_file_as_bitmap_2(Stream, BufferSize * 2,
                    Res, !BMs, !IO)
            )
        ;
            ReadRes = error(Err),
            Res = error(Err)
        )
    ).

%-----------------------------------------------------------------------------%

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

io.read_line_as_string(input_stream(Stream), Result, !IO) :-
    io.read_line_as_string_2(Stream, yes, Res, String, !IO),
    ( Res < 0 ->
        ( Res = -1 ->
            Result = eof
        ; Res = -2 ->
            Result = error(io_error("null character in input"))
        ;
            io.make_err_msg("read failed: ", Msg, !IO),
            Result = error(io_error(Msg))
        )
    ;
        Result = ok(String)
    ).

:- pred io.read_line_as_string_2(io.stream::in, bool::in, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.read_line_as_string_2(Stream::in, _FirstCall::in, Res::out,
        RetString::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#define ML_IO_READ_LINE_GROW(n) ((n) * 3 / 2)
#define ML_IO_BYTES_TO_WORDS(n) (((n) + sizeof(MR_Word) - 1) / sizeof(MR_Word))
#define ML_IO_READ_LINE_START   1024

    char initial_read_buffer[ML_IO_READ_LINE_START];
    char *read_buffer = initial_read_buffer;
    size_t read_buf_size = ML_IO_READ_LINE_START;
    size_t i;
    int char_code = '\\0';

    Res = 0;
    for (i = 0; char_code != '\\n'; ) {
        char_code = mercury_get_byte(Stream);
        if (char_code == EOF) {
            if (i == 0) {
                Res = -1;
            }
            break;
        }
        if (char_code == 0) {
            Res = -2;
            break;
        }
        read_buffer[i++] = char_code;
        MR_assert(i <= read_buf_size);
        if (i == read_buf_size) {
            /* Grow the read buffer */
            read_buf_size = ML_IO_READ_LINE_GROW(read_buf_size);
            if (read_buffer == initial_read_buffer) {
                read_buffer = MR_NEW_ARRAY(char, read_buf_size);
                MR_memcpy(read_buffer, initial_read_buffer,
                    ML_IO_READ_LINE_START);
            } else {
                read_buffer = MR_RESIZE_ARRAY(read_buffer, char,
                    read_buf_size);
            }
        }
    }
    if (Res == 0) {
        MR_Word ret_string_word;
        MR_offset_incr_hp_atomic_msg(ret_string_word,
            0, ML_IO_BYTES_TO_WORDS((i + 1) * sizeof(char)),
            MR_ALLOC_ID, ""string.string/0"");
        RetString = (MR_String) ret_string_word;
        MR_memcpy(RetString, read_buffer, i * sizeof(char));
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
").

:- pragma foreign_proc("Java",
    io.read_line_as_string_2(Stream::in, _FirstCall::in, Res::out,
        RetString::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate],
"
    try {
        RetString = ((io.MR_TextInputFile) Stream).read_line();
        Res = (RetString != null) ? 0 : -1;
    } catch (java.io.IOException e) {
        io.MR_io_exception.set(e);
        Res = -3;
        RetString = """";
    }
").

io.read_line_as_string_2(Stream, FirstCall, Res, String, !IO) :-
    % XXX This is terribly inefficient, a better approach would be to
    % use a buffer like what is done for io.read_file_as_string.
    io.read_char(input_stream(Stream), Result, !IO),
    (
        Result = ok(Char),
        ( Char = '\n' ->
            Res = 0,
            String = "\n"
        ; char.to_int(Char, 0) ->
            Res = -2,
            String = ""
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
        Res = -3
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

:- pragma foreign_proc("Java",
    io.read_file_as_string(InputStream::in, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    io.MR_TextInputFile File;
    StringBuilder sb;

    File = (io.MR_TextInputFile) InputStream.F1;
    sb = new StringBuilder();

    try {
        File.read_file(sb);
        Result = ML_make_io_maybe_partial_res_1_ok_string(sb.toString());
    } catch (java.io.IOException e) {
        Result = ML_make_io_maybe_partial_res_1_error_string(sb.toString(),
            e, ""io.read_file_as_string failed: "");
    }
").

:- pragma foreign_proc("Erlang",
    io.read_file_as_string(InputStream::in, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {input_stream, Stream} = InputStream,
    Result = mercury__io:mercury_read_string_to_eof(Stream)
").

io.read_file_as_string(Stream, Result, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    io.input_stream_file_size(Stream, FileSize, !IO),
    ( FileSize >= 0 ->
        BufferSize0 = FileSize + 1
    ;
        BufferSize0 = 4000
    ),
    io.alloc_buffer(BufferSize0, Buffer0),

    % Read the file into the buffer (resizing it as we go if necessary),
    % convert the buffer into a string, and see if anything went wrong.
    io.input_clear_err(Stream, !IO),
    Pos0 = 0,
    io.read_file_as_string_2(Stream, Buffer0, Buffer, Pos0, Pos,
        BufferSize0, BufferSize, !IO),
    require(Pos < BufferSize, "io.read_file_as_string: overflow"),
    ( io.buffer_to_string(Buffer, Pos, String) ->
        io.input_check_err(Stream, Result0, !IO),
        (
            Result0 = ok,
            Result = ok(String)
        ;
            Result0 = error(Error),
            Result = error(String, Error)
        )
    ;
        Result = error("", io_error("null character in input"))
    ).

:- pred io.read_file_as_string_2(io.input_stream::in, buffer::buffer_di,
    buffer::buffer_uo, int::in, int::out, int::in, int::out, io::di, io::uo)
    is det.

io.read_file_as_string_2(Stream, !Buffer, !Pos, !Size, !IO) :-
    Pos0 = !.Pos,
    Size0 = !.Size,
    Stream = input_stream(RealStream),
    io.read_into_buffer(RealStream, !Buffer, !Pos, !.Size, !IO),
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

:- pred io.input_clear_err(io.input_stream::in, io::di, io::uo) is det.

io.input_clear_err(input_stream(Stream), !IO) :-
    io.clear_err(Stream, !IO).

:- pred io.output_clear_err(io.output_stream::in, io::di, io::uo) is det.

io.output_clear_err(output_stream(Stream), !IO) :-
    io.clear_err(Stream, !IO).

    % Same as ANSI C's clearerr().
    %
:- pred io.clear_err(stream::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.clear_err(Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (MR_IS_FILE_STREAM(*Stream)) {
        clearerr(MR_file(*Stream));
    } else {
        /* Not a file stream so do nothing */
    }
").

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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    // XXX as for .NET above
").

:- pragma foreign_proc("Erlang",
    io.clear_err(_Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX as for .NET above
    void
").

:- pred io.input_check_err(io.input_stream::in, io.res::out, io::di, io::uo)
    is det.

io.input_check_err(input_stream(Stream), Result, !IO) :-
    io.check_err(Stream, Result, !IO).

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
    ferror(Stream::in, RetVal::out, RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (MR_IS_FILE_STREAM(*Stream)) {
        RetVal = ferror(MR_file(*Stream));
    } else {
        RetVal = -1;
    }

    ML_maybe_make_err_msg(RetVal != 0, errno, ""read failed: "",
        MR_ALLOC_ID, MR_TRUE, RetStr);
").

:- pragma foreign_proc("C#",
    ferror(_Stream::in, RetVal::out, _RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    // XXX see clearerr
    RetVal = 0;
}").

:- pragma foreign_proc("Java",
    ferror(_Stream::in, RetVal::out, _RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"{
    // XXX see clearerr
    RetVal = 0;
}").

:- pragma foreign_proc("Erlang",
    ferror(_Stream::in, RetVal::out, RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX see clearerr
    RetVal = 0,
    RetStr = <<>>
").

:- pred io.make_err_msg(string::in, string::out, io::di, io::uo) is det.

io.make_err_msg(Msg0, Msg, !IO) :-
    io.get_system_error(Error, !IO),
    io.make_err_msg(Error, Msg0, Msg, !IO).

:- pred io.get_system_error(io.system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.get_system_error(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"{
    /*
    ** XXX If the Mercury context that called the failing C function is now
    ** running on a different OS thread, this errno won't be the one
    ** we are looking for.  Or, if a different Mercury context was run on
    ** the same thread in the meantime, the errno could have been clobbered.
    */
    Error = errno;
}").

:- pragma foreign_proc("C#",
    io.get_system_error(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    Error = io.MR_io_exception;
}").

:- pragma foreign_proc("Java",
    io.get_system_error(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Error = io.MR_io_exception.get();
").

:- pragma foreign_proc("Erlang",
    io.get_system_error(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Error = get('MR_io_exception')
").

:- pragma foreign_export("C", make_err_msg(in, in, out, di, uo),
    "ML_make_err_msg").
:- pragma foreign_export("IL", make_err_msg(in, in, out, di, uo),
    "ML_make_err_msg").
:- pragma foreign_export("C#", make_err_msg(in, in, out, di, uo),
    "ML_make_err_msg").

:- pragma foreign_proc("C",
    make_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ML_maybe_make_err_msg(MR_TRUE, Error, Msg0, MR_ALLOC_ID, MR_FALSE, Msg);
").

:- pragma foreign_proc("C#",
    make_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"{
    Msg = System.String.Concat(Msg0, Error.Message);
}").

:- pragma foreign_proc("Java",
    make_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    if (Error.getMessage() != null) {
        Msg = Msg0 + Error.getMessage();
    } else {
        Msg = Msg0;
    }
").

:- pragma foreign_proc("Erlang",
    make_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    case Error of
        undefined ->
            Msg = Msg0;
        Reason ->
            Msg = list_to_binary([Msg0, file:format_error(Reason)])
    end
").

have_win32 :- semidet_fail.

:- pragma foreign_proc("C",
    have_win32,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_WIN32)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

have_cygwin :- semidet_fail.

:- pragma foreign_proc("C",
    have_cygwin,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_CYGWIN)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

have_dotnet :-
    semidet_fail.

:- pragma foreign_proc("C#",
    have_dotnet,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_export("C", make_win32_err_msg(in, in, out, di, uo),
    "ML_make_win32_err_msg").
:- pragma foreign_export("IL", make_win32_err_msg(in, in, out, di, uo),
    "ML_make_win32_err_msg").
:- pragma foreign_export("C#", make_win32_err_msg(in, in, out, di, uo),
    "ML_make_win32_err_msg").

make_win32_err_msg(_, _, "", !IO) :-
    ( semidet_succeed ->
        error("io.make_win32_err_msg called for non Win32 back-end")
    ;
        true
    ).

:- pragma foreign_proc("C",
    make_win32_err_msg(Error::in, Msg0::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ML_maybe_make_win32_err_msg(MR_TRUE, Error, Msg0, MR_ALLOC_ID, Msg);
").

make_maybe_win32_err_msg(Error, Msg0, Msg, !IO) :-
    ( have_win32 ->
        make_win32_err_msg(Error, Msg0, Msg, !IO)
    ;
        make_err_msg(Error, Msg0, Msg, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred io.input_stream_file_size(io.input_stream::in, int::out,
    io::di, io::uo) is det.

io.input_stream_file_size(input_stream(Stream), Size, !IO) :-
    io.stream_file_size(Stream, Size, !IO).

:- pred io.binary_input_stream_file_size(io.binary_input_stream::in, int::out,
    io::di, io::uo) is det.

io.binary_input_stream_file_size(binary_input_stream(Stream), Size, !IO) :-
    io.stream_file_size(Stream, Size, !IO).

:- pred io.output_stream_file_size(io.output_stream::in, int::out,
    io::di, io::uo) is det.

io.output_stream_file_size(output_stream(Stream), Size, !IO) :-
    io.stream_file_size(Stream, Size, !IO).

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
    io.stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
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
").

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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Size = ((MR_BinaryFile) Stream).size();
    } catch (java.io.IOException e) {
        Size = -1;
    }
").

:- pragma foreign_proc("Erlang",
    io.stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Size = mercury__io:mercury_get_file_size(Stream)
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
        Time::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_STAT
  #ifdef MR_WIN32
    struct _stat s;
    int stat_result = _wstat(ML_utf8_to_wide(FileName), &s);
  #else
    struct stat s;
    int stat_result = stat(FileName, &s);
  #endif

    if (stat_result == 0) {
        Time = ML_construct_time_t(s.st_mtime);
        Msg = MR_string_const("""", 0);
        Status = 1;
    } else {
        ML_maybe_make_err_msg(MR_TRUE, errno, ""stat() failed: "",
            MR_ALLOC_ID, MR_TRUE, Msg);
        Status = 0;
    }
#else
    Status = 0;
    Msg = MR_make_string_const(
        ""io.file_modification_time not available on this platform"");
#endif
").

:- pragma foreign_proc("C#",
    io.file_modification_time_2(FileName::in, Status::out, Msg::out,
        Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    try {
        if (System.IO.File.Exists(FileName)) {
            System.DateTime t = System.IO.File.GetLastWriteTime(FileName);
            Time = time.ML_construct_time_t(t);
            Msg = """";
            Status = 1;
        } else {
            Msg = ""File not found"";
            Time = null;
            Status = 0;
        }

    } catch (System.Exception e) {
        Msg = ""GetLastWriteTime() failed: "" + e.Message;
        Time = null;
        Status = 0;
    }
}").

:- pragma foreign_proc("Java",
    io.file_modification_time_2(FileName::in, Status::out, Msg::out,
        Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
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
    Time = time.ML_construct_time_t(date);
").

:- pragma foreign_proc("Erlang",
    io.file_modification_time_2(FileName::in, Status::out, Msg::out,
        Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    FileNameStr = binary_to_list(FileName),
    case filelib:last_modified(FileNameStr) of
        {YMD, HMS} ->
            Status = 1,
            Msg = <<>>,
            % time_t in Erlang is in UTC.
            Time = {time_t, erlang:localtime_to_universaltime({YMD, HMS})};
        _ ->
            Status = 0,
            Msg = <<""filelib:last_modified failed"">>,
            Time = -1
    end
").

%-----------------------------------------------------------------------------%

io.file_type(FollowSymLinks, FileName, MaybeType, !IO) :-
    ( file_type_implemented ->
        (
            FollowSymLinks = yes,
            FollowSymLinksInt = 1
        ;
            FollowSymLinks = no,
            FollowSymLinksInt = 0
        ),
        io.file_type_2(FollowSymLinksInt, FileName, MaybeType, !IO)
    ;
        MaybeType = error(io.make_io_error(
            "Sorry, io.file_type not implemented on this platform"))
    ).

:- pred file_type_implemented is semidet.

file_type_implemented :-
    semidet_fail.

:- pragma foreign_proc("C",
    file_type_implemented,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
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
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Java",
    file_type_implemented,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Erlang",
    file_type_implemented,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true
").

:- pred io.file_type_2(int::in, string::in, io.res(io.file_type)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.file_type_2(FollowSymLinks::in, FileName::in, Result::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_STAT
  #ifdef MR_WIN32
    struct _stat s;
    int stat_result = _wstat(ML_utf8_to_wide(FileName), &s);
  #else
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
  #endif

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
").

:- pragma foreign_proc("C#",
    io.file_type_2(_FollowSymLinks::in, FileName::in, Result::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    try {
        System.IO.FileAttributes attrs =
            System.IO.File.GetAttributes(FileName);
        if ((attrs & System.IO.FileAttributes.Directory) ==
            System.IO.FileAttributes.Directory)
        {
            Result = io.ML_make_io_res_1_ok_file_type(
                io.ML_file_type_directory());
        }
        else if ((attrs & System.IO.FileAttributes.Device) ==
            System.IO.FileAttributes.Device)
        {
            // XXX It may be a block device, but .NET doesn't
            // distinguish between character and block devices.
            Result = io.ML_make_io_res_1_ok_file_type(
                io.ML_file_type_character_device());
        }
        else
        {
            Result = io.ML_make_io_res_1_ok_file_type(
                io.ML_file_type_regular());
        }
    } catch (System.Exception e) {
        Result = io.ML_make_io_res_1_error_file_type(e,
            ""can't find file type: "");
    }
").

:- pragma foreign_proc("Java",
    io.file_type_2(_FollowSymLinks::in, FileName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    java.io.File file = new java.io.File(FileName);

    // The Java implementation can distinguish between regular files and
    // directories, and for everything else it just returns unknown.

    if (file.isFile()) {
        Result = new io.Res_1.Ok_1(ML_file_type_regular());
    } else if (file.isDirectory()) {
        Result = new io.Res_1.Ok_1(ML_file_type_directory());
    } else if (file.exists()) {
        Result = new io.Res_1.Ok_1(ML_file_type_unknown());
    } else {
        Result = io.ML_make_io_res_1_error_file_type(
            new java.lang.Exception(""No such file or directory""),
            ""io.file_type failed: "");
    }
").

:- pragma foreign_decl("Erlang", local, "
-include_lib(""kernel/include/file.hrl"").
").

:- pragma foreign_proc("Erlang",
    io.file_type_2(FollowSymLinks::in, FileName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    FileNameStr = binary_to_list(FileName),
    case FollowSymLinks of
        0 -> Read = fun file:read_link_info/1;
        1 -> Read = fun file:read_file_info/1
    end,
    case Read(FileNameStr) of
        {ok, FileInfo} ->
            #file_info{type = Type} = FileInfo,
            case Type of
                device ->
                    % XXX It may be a block device, but Erlang doesn't
                    % distinguish between character and block devices.
                    Result = mercury__io:'ML_make_io_res_1_ok_file_type'(
                        mercury__io:'ML_file_type_character_device'());
                directory ->
                    Result = mercury__io:'ML_make_io_res_1_ok_file_type'(
                        mercury__io:'ML_file_type_directory'());
                regular ->
                    Result = mercury__io:'ML_make_io_res_1_ok_file_type'(
                        mercury__io:'ML_file_type_regular'());
                symlink ->
                    Result = mercury__io:'ML_make_io_res_1_ok_file_type'(
                        mercury__io:'ML_file_type_symbolic_link'());
                other ->
                    Result = mercury__io:'ML_make_io_res_1_ok_file_type'(
                        mercury__io:'ML_file_type_unknown'())
            end;
        {error, Reason} ->
            Result = mercury__io:'ML_make_io_res_1_error_file_type'(Reason,
                ""io.file_type failed: "")
    end
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

:- pragma foreign_export("C", file_type_character_device = out,
    "ML_file_type_character_device").
:- pragma foreign_export("IL", file_type_character_device = out,
    "ML_file_type_character_device").
:- pragma foreign_export("C#", file_type_character_device = out,
    "ML_file_type_character_device").
:- pragma foreign_export("Erlang", file_type_character_device = out,
    "ML_file_type_character_device").
:- pragma foreign_export("C", file_type_block_device = out,
    "ML_file_type_block_device").
:- pragma foreign_export("IL", file_type_block_device = out,
    "ML_file_type_block_device").
:- pragma foreign_export("C#", file_type_block_device = out,
    "ML_file_type_block_device").
:- pragma foreign_export("C", file_type_fifo = out,
    "ML_file_type_fifo").
:- pragma foreign_export("IL", file_type_fifo = out,
    "ML_file_type_fifo").
:- pragma foreign_export("C#", file_type_fifo = out,
    "ML_file_type_fifo").
:- pragma foreign_export("C", file_type_directory = out,
    "ML_file_type_directory").
:- pragma foreign_export("IL", file_type_directory = out,
    "ML_file_type_directory").
:- pragma foreign_export("C#", file_type_directory = out,
    "ML_file_type_directory").
:- pragma foreign_export("Java", file_type_directory = out,
    "ML_file_type_directory").
:- pragma foreign_export("Erlang", file_type_directory = out,
    "ML_file_type_directory").
:- pragma foreign_export("C", file_type_socket = out,
    "ML_file_type_socket").
:- pragma foreign_export("IL", file_type_socket = out,
    "ML_file_type_socket").
:- pragma foreign_export("C#", file_type_socket = out,
    "ML_file_type_socket").
:- pragma foreign_export("C", file_type_symbolic_link = out,
    "ML_file_type_symbolic_link").
:- pragma foreign_export("IL", file_type_symbolic_link = out,
    "ML_file_type_symbolic_link").
:- pragma foreign_export("C#", file_type_symbolic_link = out,
    "ML_file_type_symbolic_link").
:- pragma foreign_export("Erlang", file_type_symbolic_link = out,
    "ML_file_type_symbolic_link").
:- pragma foreign_export("C", file_type_regular = out,
    "ML_file_type_regular").
:- pragma foreign_export("IL", file_type_regular = out,
    "ML_file_type_regular").
:- pragma foreign_export("C#", file_type_regular = out,
    "ML_file_type_regular").
:- pragma foreign_export("Java", file_type_regular = out,
    "ML_file_type_regular").
:- pragma foreign_export("Erlang", file_type_regular = out,
    "ML_file_type_regular").
:- pragma foreign_export("C", file_type_message_queue = out,
    "ML_file_type_message_queue").
:- pragma foreign_export("IL", file_type_message_queue = out,
    "ML_file_type_message_queue").
:- pragma foreign_export("C#", file_type_message_queue = out,
    "ML_file_type_message_queue").
:- pragma foreign_export("C", file_type_semaphore = out,
    "ML_file_type_semaphore").
:- pragma foreign_export("IL", file_type_semaphore = out,
    "ML_file_type_semaphore").
:- pragma foreign_export("C#", file_type_semaphore = out,
    "ML_file_type_semaphore").
:- pragma foreign_export("C", file_type_shared_memory = out,
    "ML_file_type_shared_memory").
:- pragma foreign_export("IL", file_type_shared_memory = out,
    "ML_file_type_shared_memory").
:- pragma foreign_export("C#", file_type_shared_memory = out,
    "ML_file_type_shared_memory").
:- pragma foreign_export("C", file_type_unknown = out,
    "ML_file_type_unknown").
:- pragma foreign_export("IL", file_type_unknown = out,
    "ML_file_type_unknown").
:- pragma foreign_export("C#", file_type_unknown = out,
    "ML_file_type_unknown").
:- pragma foreign_export("Java", file_type_unknown = out,
    "ML_file_type_unknown").
:- pragma foreign_export("Erlang", file_type_unknown = out,
    "ML_file_type_unknown").

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
    io.check_file_accessibility_2(FileName::in, AccessTypes::in, Result::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_HAVE_ACCESS)
  #ifdef F_OK
    int mode = F_OK;
  #else
    int mode = 0;
  #endif
    int access_result;

  #if !defined(MR_WIN32) || defined(MR_CYGWIN)
    /*
    ** Earlier versions of MSVCRT ignored flags it doesn't support,
    ** later versions return an error (e.g. on Vista).
    */
    if (ML_access_types_includes_execute(AccessTypes)) {
      #ifdef X_OK
        mode |= X_OK;
      #else
        mode |= 1;
      #endif
    }
  #endif
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

  #ifdef MR_WIN32
    access_result = _waccess(ML_utf8_to_wide(FileName), mode);
  #else
    access_result = access(FileName, mode);
  #endif

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
").

:- pragma foreign_proc("Java",
    io.check_file_accessibility_2(FileName::in, AccessTypes::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    java.io.File file = new java.io.File(FileName);
    try {
        boolean ok = true;

        if (ML_access_types_includes_read(AccessTypes)) {
            ok = file.canRead();
        }

        if (ok && ML_access_types_includes_write(AccessTypes)) {
            ok = file.canWrite();
        }

        if (ok && ML_access_types_includes_execute(AccessTypes)) {
            // File.canExecute() was added in Java 1.6 but we only require
            // Java 1.5.
            try {
                java.lang.reflect.Method canExecute =
                    file.getClass().getMethod(""canExecute"");
                ok = (Boolean) canExecute.invoke(file);
            }
            catch (java.lang.NoSuchMethodException e) {
                // Assume the file is executable.
            }
            catch (java.lang.IllegalAccessException e) {
                // Assume the file is executable.
            }
            catch (java.lang.reflect.InvocationTargetException e) {
                ok = false;
            }
        }

        if (ok) {
            Result = ML_make_io_res_0_ok();
        } else {
            Result = ML_make_io_res_0_error_msg(
                ""file not accessible: Permission denied"");
        }
    }
    catch (java.lang.SecurityException e) {
        Result = ML_make_io_res_0_error_msg(e.toString());
    }
").

:- pragma foreign_proc("Erlang",
    io.check_file_accessibility_2(FileName::in, AccessTypes::in, Result::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, may_not_duplicate],
"
    FileNameStr = binary_to_list(FileName),
    case file:read_file_info(FileNameStr) of
        {ok, FileInfo} ->
            Access = FileInfo#file_info.access,
            case mercury__io:'ML_access_types_includes_read'(AccessTypes) of
                {} ->
                    Ok0 = lists:member(Access, [read, read_write]);
                fail ->
                    Ok0 = true
            end,
            case Ok0 of
                true ->
                    case
                        mercury__io:'ML_access_types_includes_write'(
                            AccessTypes)
                    of
                        {} ->
                            Ok = lists:member(Access, [write, read_write]);
                        fail ->
                            Ok = true
                    end;
                false ->
                    Ok = Ok0
            end,
            % XXX test execute access somehow
            case Ok of
                true ->
                    Result = mercury__io:'ML_make_io_res_0_ok'();
                false ->
                    Result = mercury__io:'ML_make_io_res_0_error'(eacces,
                        <<""file not accessible: "">>)
            end
    ;
        {error, Reason} ->
            Result = mercury__io:'ML_make_io_res_0_error'(Reason,
                <<""file not accessible: "">>)
    end
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
            (
                CheckRead = yes,
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
                CheckRead = no,
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
        Result = io.ML_make_io_res_0_ok();
    } catch (System.Exception e) {
        Result = io.ML_make_io_res_0_error(e,
            ""execute permission check failed: "");
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
        Result = io.ML_make_io_res_0_ok();
    } catch (System.Exception e) {
        Result = io.ML_make_io_res_0_error(e, ""permission check failed: "");
    }
}").

:- pred access_types_includes_read(list(access_type)::in) is semidet.
:- pragma foreign_export("C", access_types_includes_read(in),
    "ML_access_types_includes_read").
:- pragma foreign_export("IL", access_types_includes_read(in),
    "ML_access_types_includes_read").
:- pragma foreign_export("C#", access_types_includes_read(in),
    "ML_access_types_includes_read").
:- pragma foreign_export("Java", access_types_includes_read(in),
    "ML_access_types_includes_read").
:- pragma foreign_export("Erlang", access_types_includes_read(in),
    "ML_access_types_includes_read").

access_types_includes_read(Access) :-
    list.member(read, Access).

:- pred access_types_includes_write(list(access_type)::in) is semidet.
:- pragma foreign_export("C", access_types_includes_write(in),
    "ML_access_types_includes_write").
:- pragma foreign_export("IL", access_types_includes_write(in),
    "ML_access_types_includes_write").
:- pragma foreign_export("C#", access_types_includes_write(in),
    "ML_access_types_includes_write").
:- pragma foreign_export("Java", access_types_includes_write(in),
    "ML_access_types_includes_write").
:- pragma foreign_export("Erlang", access_types_includes_write(in),
    "ML_access_types_includes_write").

access_types_includes_write(Access) :-
    list.member(write, Access).

:- pred access_types_includes_execute(list(access_type)::in) is semidet.
:- pragma foreign_export("C", access_types_includes_execute(in),
    "ML_access_types_includes_execute").
:- pragma foreign_export("IL", access_types_includes_execute(in),
    "ML_access_types_includes_execute").
:- pragma foreign_export("C#", access_types_includes_execute(in),
    "ML_access_types_includes_execute").
:- pragma foreign_export("Java", access_types_includes_execute(in),
    "ML_access_types_includes_execute").
:- pragma foreign_export("Erlang", access_types_includes_execute(in),
    "ML_access_types_includes_execute").

access_types_includes_execute(Access) :-
    list.member(execute, Access).

:- func make_io_res_0_ok = io.res.
:- pragma foreign_export("C", (make_io_res_0_ok = out),
    "ML_make_io_res_0_ok").
:- pragma foreign_export("IL", (make_io_res_0_ok = out),
    "ML_make_io_res_0_ok").
:- pragma foreign_export("C#", (make_io_res_0_ok = out),
    "ML_make_io_res_0_ok").
:- pragma foreign_export("Java", (make_io_res_0_ok = out),
    "ML_make_io_res_0_ok").
:- pragma foreign_export("Erlang", (make_io_res_0_ok = out),
    "ML_make_io_res_0_ok").

make_io_res_0_ok = ok.

:- pred make_io_res_0_error(io.system_error::in, string::in, io.res::out,
    io::di, io::uo) is det.
:- pragma foreign_export("C", make_io_res_0_error(in, in, out, di, uo),
    "ML_make_io_res_0_error").
:- pragma foreign_export("IL", make_io_res_0_error(in, in, out, di, uo),
    "ML_make_io_res_0_error").
:- pragma foreign_export("C#", make_io_res_0_error(in, in, out, di, uo),
    "ML_make_io_res_0_error").
:- pragma foreign_export("Java", make_io_res_0_error(in, in, out, di, uo),
    "ML_make_io_res_0_error").
:- pragma foreign_export("Erlang", make_io_res_0_error(in, in, out, di, uo),
    "ML_make_io_res_0_error").

make_io_res_0_error(Error, Msg0, error(make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, Msg0, Msg, !IO).

:- func make_io_res_0_error_msg(string) = io.res.
:- pragma foreign_export("C", (make_io_res_0_error_msg(in) = out),
    "ML_make_io_res_0_error_msg").
:- pragma foreign_export("IL", (make_io_res_0_error_msg(in) = out),
    "ML_make_io_res_0_error_msg").
:- pragma foreign_export("C#", (make_io_res_0_error_msg(in) = out),
    "ML_make_io_res_0_error_msg").
:- pragma foreign_export("Java", (make_io_res_0_error_msg(in) = out),
    "ML_make_io_res_0_error_msg").

make_io_res_0_error_msg(Msg) = error(make_io_error(Msg)).

:- func make_io_res_1_ok_file_type(file_type) = io.res(file_type).
:- pragma foreign_export("C", (make_io_res_1_ok_file_type(in) = out),
    "ML_make_io_res_1_ok_file_type").
:- pragma foreign_export("IL", (make_io_res_1_ok_file_type(in) = out),
    "ML_make_io_res_1_ok_file_type").
:- pragma foreign_export("C#", (make_io_res_1_ok_file_type(in) = out),
    "ML_make_io_res_1_ok_file_type").
:- pragma foreign_export("Java", (make_io_res_1_ok_file_type(in) = out),
    "ML_make_io_res_1_ok_file_type").
:- pragma foreign_export("Erlang", (make_io_res_1_ok_file_type(in) = out),
    "ML_make_io_res_1_ok_file_type").

make_io_res_1_ok_file_type(FileType) = ok(FileType).

:- pred make_io_res_1_error_file_type(io.system_error::in,
    string::in, io.res(file_type)::out, io::di, io::uo) is det.
:- pragma foreign_export("C",
    make_io_res_1_error_file_type(in, in, out, di, uo),
    "ML_make_io_res_1_error_file_type").
:- pragma foreign_export("IL",
    make_io_res_1_error_file_type(in, in, out, di, uo),
    "ML_make_io_res_1_error_file_type").
:- pragma foreign_export("C#",
    make_io_res_1_error_file_type(in, in, out, di, uo),
    "ML_make_io_res_1_error_file_type").
:- pragma foreign_export("Java",
    make_io_res_1_error_file_type(in, in, out, di, uo),
    "ML_make_io_res_1_error_file_type").
:- pragma foreign_export("Erlang",
    make_io_res_1_error_file_type(in, in, out, di, uo),
    "ML_make_io_res_1_error_file_type").

make_io_res_1_error_file_type(Error, Msg0, error(make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, Msg0, Msg, !IO).

:- func make_io_res_1_ok_string(string) = io.res(string).
:- pragma foreign_export("C", (make_io_res_1_ok_string(in) = out),
    "ML_make_io_res_1_ok_string").
:- pragma foreign_export("C#", (make_io_res_1_ok_string(in) = out),
    "ML_make_io_res_1_ok_string").
:- pragma foreign_export("Java", (make_io_res_1_ok_string(in) = out),
    "ML_make_io_res_1_ok_string").
:- pragma foreign_export("Erlang", (make_io_res_1_ok_string(in) = out),
    "ML_make_io_res_1_ok_string").

make_io_res_1_ok_string(String) = ok(String).

:- pred make_io_res_1_error_string(io.system_error::in,
    string::in, io.res(string)::out, io::di, io::uo) is det.
:- pragma foreign_export("C",
    make_io_res_1_error_string(in, in, out, di, uo),
    "ML_make_io_res_1_error_string").
:- pragma foreign_export("C#",
    make_io_res_1_error_string(in, in, out, di, uo),
    "ML_make_io_res_1_error_string").
:- pragma foreign_export("Java",
    make_io_res_1_error_string(in, in, out, di, uo),
    "ML_make_io_res_1_error_string").
:- pragma foreign_export("Erlang",
    make_io_res_1_error_string(in, in, out, di, uo),
    "ML_make_io_res_1_error_string").

make_io_res_1_error_string(Error, Msg0, error(make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, Msg0, Msg, !IO).

:- func make_io_maybe_partial_res_1_ok_string(string)
    = io.maybe_partial_res(string).
:- pragma foreign_export("Java",
    (make_io_maybe_partial_res_1_ok_string(in) = out),
    "ML_make_io_maybe_partial_res_1_ok_string").

make_io_maybe_partial_res_1_ok_string(String) = ok(String).

:- pred make_io_maybe_partial_res_1_error_string(string::in,
    io.system_error::in, string::in, io.maybe_partial_res(string)::out,
    io::di, io::uo) is det.
:- pragma foreign_export("Java",
    make_io_maybe_partial_res_1_error_string(in, in, in, out, di, uo),
    "ML_make_io_maybe_partial_res_1_error_string").

make_io_maybe_partial_res_1_error_string(Partial, Error, Msg0, Res, !IO) :-
    io.make_err_msg(Error, Msg0, Msg, !IO),
    Res = error(Partial, make_io_error(Msg)).

%-----------------------------------------------------------------------------%

:- type file_id ---> file_id.
:- pragma foreign_type("C", file_id, "ML_File_Id")
    where comparison is compare_file_id.
:- pragma foreign_type("Erlang", file_id, "")
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
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
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
        if (inode_cmp < 0) {
            Res = -1;
        } else if (inode_cmp > 0) {
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
    io.ML_throw_io_error(""File IDs are not supported by Java."");
").

:- pragma foreign_proc("Erlang",
    compare_file_id_2(Res::out, FileId1::in, FileId2::in),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    if
        FileId1 =:= FileId2 ->
            Res = 0;
        FileId1  <  FileId2 ->
            Res = -1;
        true ->
            Res = 1
    end
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
        FileId::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_STAT
  #ifdef MR_WIN32
    struct _stat s;
    int stat_result = _wstat(ML_utf8_to_wide(FileName), &s);
  #else
    struct stat s;
    int stat_result = stat(FileName, &s);
  #endif

    if (stat_result == 0) {
        FileId.device = s.st_dev;
        FileId.inode = s.st_ino;
        Msg = MR_string_const("""", 0);
        Status = 1;
    } else {
        ML_maybe_make_err_msg(MR_TRUE, errno, ""stat() failed: "",
            MR_ALLOC_ID, MR_TRUE, Msg);
        Status = 0;
    }
#else
    MR_fatal_error(""io.file_id_2 called but not supported"");
#endif
").

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

:- pragma foreign_proc("Erlang",
    io.file_id_2(FileName::in, Status::out, Msg::out,
        FileId::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    FileNameStr = binary_to_list(FileName),
    case file:read_file_info(FileNameStr) of
        {ok, FileInfo} ->
            MajorDevice = FileInfo#file_info.major_device,
            Inode = FileInfo#file_info.inode,
            FileId = {MajorDevice, Inode},
            Msg = <<>>,
            Status = 1;
        {error, Reason} ->
            FileId = null,
            Msg = list_to_binary(file:format_error(Reason)),
            Status = 0
    end
").

% Can we retrieve inode numbers on this system.
have_file_ids :- semidet_fail.
:- pragma foreign_proc("C",
    have_file_ids,
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_BROKEN_STAT_ST_INO) || !defined(MR_HAVE_STAT)
    /* Win32 returns junk in the st_ino field of `struct stat'. */
    SUCCESS_INDICATOR = MR_FALSE;
#else
    SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("Erlang",
    have_file_ids,
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    SUCCESS_INDICATOR = true
").

%-----------------------------------------------------------------------------%

% A `buffer' is just an array of Chars.
% Buffer sizes are measured in Chars.

:- type buffer.
:- pragma foreign_type(c, buffer, "char *", [can_pass_as_mercury_type]).

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
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"{
    MR_Word buf;
    MR_offset_incr_hp_atomic_msg(buf, 0,
        (Size * sizeof(char) + sizeof(MR_Word) - 1) / sizeof(MR_Word),
        MR_ALLOC_ID, ""io.buffer/0"");
    Buffer = (char *) buf;
}").

io.alloc_buffer(Size, buffer(Array)) :-
    char.det_from_int(0, NullChar),
    array.init(Size, NullChar, Array).

:- pred io.resize_buffer(int::in, int::in,
    buffer::buffer_di, buffer::buffer_uo) is det.

:- pragma foreign_proc("C",
    io.resize_buffer(OldSize::in, NewSize::in,
        Buffer0::buffer_di, Buffer::buffer_uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"{
    MR_CHECK_EXPR_TYPE(Buffer0, char *);
    MR_CHECK_EXPR_TYPE(Buffer, char *);

#ifdef MR_CONSERVATIVE_GC
    Buffer = MR_GC_realloc(Buffer0, NewSize * sizeof(char));
#else
    if (Buffer0 + OldSize == (char *) MR_hp) {
        MR_Word next;
        MR_offset_incr_hp_atomic_msg(next, 0,
            (NewSize * sizeof(char) + sizeof(MR_Word) - 1)
                / sizeof(MR_Word),
            MR_ALLOC_ID, ""io.buffer/0"");
        assert(Buffer0 + OldSize == (char *) next);
        Buffer = Buffer0;
    } else {
        /* just have to alloc and copy */
        MR_Word buf;
        MR_offset_incr_hp_atomic_msg(buf, 0,
            (NewSize * sizeof(char) + sizeof(MR_Word) - 1)
                / sizeof(MR_Word),
            MR_ALLOC_ID, ""io.buffer/0"");
        Buffer = (char *) buf;
        if (OldSize > NewSize) {
            MR_memcpy(Buffer, Buffer0, NewSize);
        } else {
            MR_memcpy(Buffer, Buffer0, OldSize);
        }
    }
#endif
}").

io.resize_buffer(_OldSize, NewSize, buffer(Array0), buffer(Array)) :-
    char.det_from_int(0, Char),
    array.resize(NewSize, Char, Array0, Array).

:- pred io.buffer_to_string(buffer::buffer_di, int::in, string::uo) is semidet.

:- pragma foreign_proc("C",
    io.buffer_to_string(Buffer::buffer_di, Len::in, Str::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"{
    int i;

    Str = Buffer;
    Str[Len] = '\\0';

    /* Check that the string doesn't contain null characters. */
    if (strlen(Str) != Len) {
        SUCCESS_INDICATOR= MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").

io.buffer_to_string(buffer(Array), Len, String) :-
    array.fetch_items(Array, min(Array), min(Array) + Len - 1, List),
    string.semidet_from_char_list(List, String).

:- pred io.read_into_buffer(stream::in, buffer::buffer_di, buffer::buffer_uo,
    int::in, int::out, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.read_into_buffer(Stream::in, Buffer0::buffer_di, Buffer::buffer_uo,
        Pos0::in, Pos::out, Size::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    int items_read;

    MR_CHECK_EXPR_TYPE(Buffer0, char *);
    MR_CHECK_EXPR_TYPE(Buffer, char *);

    items_read = MR_READ(*Stream, Buffer0 + Pos0, Size - Pos0);

    Buffer = Buffer0;
    Pos = Pos0 + items_read;
").

io.read_into_buffer(Stream, buffer(Array0), buffer(Array), !Pos, Size, !IO) :-
    io.read_into_array(Stream, Array0, Array, !Pos, Size, !IO).

:- pred io.read_into_array(stream::in,
    array(char)::array_di, array(char)::array_uo, int::in, int::out,
    int::in, io::di, io::uo) is det.

io.read_into_array(Stream, !Array, !Pos, Size, !IO) :-
    ( !.Pos >= Size ->
        true
    ;
        io.read_char(input_stream(Stream), CharResult, !IO),
        (
            CharResult = ok(Char),
            array.set(!.Pos, Char, !Array),
            !:Pos = !.Pos + 1,
            io.read_into_array(Stream, !Array, !Pos, Size, !IO)
        ;
            CharResult = error(_)
        ;
            CharResult = eof
        )
    ).

%-----------------------------------------------------------------------------%

io.read_binary_file(Result, !IO) :-
    io.binary_input_stream(Stream, !IO),
    io.read_binary_file(Stream, Result, !IO).

io.read_binary_file(Stream, Result, !IO) :-
    io.read_binary_file_2(Stream, [], Result, !IO).

:- pred io.read_binary_file_2(io.binary_input_stream::in, list(int)::in,
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
    [will_not_call_mercury, promise_pure, does_not_affect_liveness],
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
    parser.read_term_from_substring(FileName, String, Len, !Posn, ReadResult),
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
%
% Various different versions of io.print
%

:- pragma foreign_export("C", io.print(in, di, uo),
    "ML_io_print_to_cur_stream").
:- pragma foreign_export("IL", io.print(in, di, uo),
    "ML_io_print_to_cur_stream").
:- pragma foreign_export("C#", io.print(in, di, uo),
    "ML_io_print_to_cur_stream").
:- pragma foreign_export("Java", io.print(in, di, uo),
    "ML_io_print_to_cur_stream").

io.print(Term, !IO) :-
    io.output_stream(Stream, !IO),
    stream.string_writer.print(Stream, canonicalize, Term, !IO).

io.print(Stream, Term, !IO) :-
    stream.string_writer.print(Stream, canonicalize, Term, !IO).

io.print(Stream, NonCanon, Term, !IO) :-
    stream.string_writer.print(Stream, NonCanon, Term, !IO).

io.print_cc(Term, !IO) :-
    io.output_stream(Stream, !IO),
    stream.string_writer.print_cc(Stream, Term, !IO).

:- pred io.print_to_stream(io.stream::in, T::in, io::di, io::uo) is det.

:- pragma foreign_export("C", io.print_to_stream(in, in, di, uo),
    "ML_io_print_to_stream").
:- pragma foreign_export("IL", io.print_to_stream(in, in, di, uo),
    "ML_io_print_to_stream").
:- pragma foreign_export("Java", io.print_to_stream(in, in, di, uo),
    "ML_io_print_to_stream").

io.print_to_stream(Stream, Term, !IO) :-
    io.print(output_stream(Stream), canonicalize, Term, !IO).

io.print_line(Term, !IO) :-
    io.print(Term, !IO),
    io.nl(!IO).

io.print_line(Stream, Term, !IO) :-
    io.print(Stream, Term, !IO),
    io.nl(!IO).

io.print_line(Stream, NonCanon, Term, !IO) :-
    io.print(Stream, NonCanon, Term, !IO),
    io.nl(!IO).

io.print_line_cc(Term, !IO) :-
    io.print_cc(Term, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
%
% Various different versions of io.write
%

io.write(X, !IO) :-
    io.output_stream(Stream, !IO),
    stream.string_writer.write(Stream, canonicalize, X, !IO).

io.write(Stream, X, !IO) :-
    stream.string_writer.write(Stream, canonicalize, X, !IO).

io.write(Stream, NonCanon, X, !IO) :-
    stream.string_writer.write(Stream, NonCanon, X, !IO).

io.write_cc(X, !IO) :-
    io.output_stream(Stream, !IO),
    stream.string_writer.write(Stream, include_details_cc, X, !IO).

io.write_line(X, !IO) :-
    io.write(X, !IO),
    io.nl(!IO).

io.write_line(Stream, X, !IO) :-
    io.write(Stream, X, !IO),
    io.nl(!IO).

io.write_line(Stream, NonCanon, X, !IO) :-
    io.write(Stream, NonCanon, X, !IO),
    io.nl(!IO).

io.write_line_cc(X, !IO) :-
    io.write_cc(X, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

io.write_list([], _Separator, _OutputPred, !IO).
io.write_list([E | Es], Separator, OutputPred, !IO) :-
    OutputPred(E, !IO),
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
    io.binary_output_stream(binary_output_stream(Stream), !IO),
    io.write(output_stream(Stream), Term, !IO),
    io.write_string(output_stream(Stream), ".\n", !IO).

io.read_binary(Result, !IO) :-
    % A quick-and-dirty implementation... not very space-efficient
    % (not really binary!)
    % XXX This will not work for the Java back-end. See the comment at the
    % top of the MR_MercuryFileStruct class definition.
    io.binary_input_stream(binary_input_stream(Stream), !IO),
    io.read(input_stream(Stream), ReadResult, !IO),
    (
        ReadResult = ok(T),
        % We've read the newline and the trailing full stop.
        % Now skip the newline after the full stop.
        io.read_char(input_stream(Stream), NewLineRes, !IO),
        (
            NewLineRes = error(Error),
            Result = error(Error)
        ;
            NewLineRes = ok(NewLineChar),
            ( NewLineChar = '\n' ->
                Result = ok(T)
            ;
                Result = error(io_error("io.read_binary: missing newline"))
            )
        ;
            NewLineRes = eof,
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
%
% Stream predicates
%

io.open_input(FileName, Result, !IO) :-
    io.do_open_text(FileName, "r", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(input_stream(NewStream)),
        io.insert_stream_info(NewStream,
            stream(OpenCount, input, text, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open input file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_output(FileName, Result, !IO) :-
    io.do_open_text(FileName, "w", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(output_stream(NewStream)),
        io.insert_stream_info(NewStream,
            stream(OpenCount, output, text, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open output file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_append(FileName, Result, !IO) :-
    io.do_open_text(FileName, "a", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(output_stream(NewStream)),
        io.insert_stream_info(NewStream,
            stream(OpenCount, append, text, file(FileName)), !IO)
    ;
        io.make_err_msg("can't append to file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_binary_input(FileName, Result, !IO) :-
    io.do_open_binary(FileName, "rb", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(binary_input_stream(NewStream)),
        io.insert_stream_info(NewStream,
            stream(OpenCount, input, binary, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open input file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_binary_output(FileName, Result, !IO) :-
    io.do_open_binary(FileName, "wb", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(binary_output_stream(NewStream)),
        io.insert_stream_info(NewStream,
            stream(OpenCount, output, binary, file(FileName)), !IO)
    ;
        io.make_err_msg("can't open output file: ", Msg, !IO),
        Result = error(io_error(Msg))
    ).

io.open_binary_append(FileName, Result, !IO) :-
    io.do_open_binary(FileName, "ab", Result0, OpenCount, NewStream, !IO),
    ( Result0 \= -1 ->
        Result = ok(binary_output_stream(NewStream)),
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
%
% Stream name predicates
%

io.input_stream_name(Name, !IO) :-
    io.input_stream(input_stream(Stream), !IO),
    io.stream_name(Stream, Name, !IO).

io.input_stream_name(input_stream(Stream), Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

io.output_stream_name(Name, !IO) :-
    io.output_stream(output_stream(Stream), !IO),
    io.stream_name(Stream, Name, !IO).

io.output_stream_name(output_stream(Stream), Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

io.binary_input_stream_name(Name, !IO) :-
    io.binary_input_stream(binary_input_stream(Stream), !IO),
    io.stream_name(Stream, Name, !IO).

io.binary_input_stream_name(binary_input_stream(Stream), Name, !IO) :-
    io.stream_name(Stream, Name, !IO).

io.binary_output_stream_name(Name, !IO) :-
    io.binary_output_stream(binary_output_stream(Stream), !IO),
    io.stream_name(Stream, Name, !IO).

io.binary_output_stream_name(binary_output_stream(Stream), Name, !IO) :-
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
    io.lock_stream_db(!IO),
    io.get_stream_db(StreamDb, !IO),
    io.unlock_stream_db(!IO),
    ( map.search(StreamDb, get_stream_id(Stream), Info) ->
        MaybeInfo = yes(Info)
    ;
        MaybeInfo = no
    ).

io.input_stream_info(StreamDb, input_stream(Stream)) =
    io.maybe_stream_info(StreamDb, Stream).

io.output_stream_info(StreamDb, output_stream(Stream)) =
    io.maybe_stream_info(StreamDb, Stream).

io.binary_input_stream_info(StreamDb, binary_input_stream(Stream)) =
    io.maybe_stream_info(StreamDb, Stream).

io.binary_output_stream_info(StreamDb, binary_output_stream(Stream)) =
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

get_io_stream_info(StreamDB, Stream) = StreamInfo :-
    ( dynamic_cast(Stream, input_stream(IOStream0)) ->
        IOStream = IOStream0
    ; dynamic_cast(Stream, output_stream(IOStream0)) ->
        IOStream = IOStream0
    ; dynamic_cast(Stream, binary_input_stream(IOStream0)) ->
        IOStream = IOStream0
    ; dynamic_cast(Stream, binary_output_stream(IOStream0)) ->
        IOStream = IOStream0
    ; dynamic_cast(Stream, IOStream0) ->
        IOStream = IOStream0
    ;
        fail
    ),
    StreamInfo = io.maybe_stream_info(StreamDB, IOStream).

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

    % Caller must NOT hold the stream_db lock.
    %
:- pragma foreign_proc("C",
    io.get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    MR_LOCK(&ML_io_stream_db_lock, ""io.get_stream_db/1"");
    StreamDb = ML_io_stream_db;
    MR_UNLOCK(&ML_io_stream_db_lock, ""io.get_stream_db/1"");
").

    % Caller must hold the stream_db lock.
    %
:- pragma foreign_proc("C",
    io.get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness],
"
    StreamDb = ML_io_stream_db;
").

    % Caller must hold the stream_db lock.
    %
:- pred io.set_stream_db(io.stream_db::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ML_io_stream_db = StreamDb;
").

:- pred io.lock_stream_db(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.lock_stream_db(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        no_sharing],
"
    MR_LOCK(&ML_io_stream_db_lock, ""io.lock_stream_db/2"");
").

io.lock_stream_db(!IO).

:- pred io.unlock_stream_db(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.unlock_stream_db(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        no_sharing],
"
    MR_UNLOCK(&ML_io_stream_db_lock, ""io.unlock_stream_db/2"");
").

io.unlock_stream_db(!IO).

:- pragma foreign_proc("C#",
    io.get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("C#",
    io.get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("C#",
    io.set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_stream_db = StreamDb;
").

:- pragma foreign_proc("Java",
    io.get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("Java",
    io.get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("Java",
    io.set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_stream_db = StreamDb;
").

% XXX the following Erlang implementation doesn't work with multiple threads

:- pragma foreign_proc("Erlang",
    io.get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = get('ML_io_stream_db')
").

:- pragma foreign_proc("Erlang",
    io.get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = get('ML_io_stream_db')
").

:- pragma foreign_proc("Erlang",
    io.set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    put('ML_io_stream_db', StreamDb)
").

%-----------------------------------------------------------------------------%

:- pred io.insert_stream_info(io.stream::in, stream_info::in,
    io::di, io::uo) is det.

io.insert_stream_info(Stream, Name, !IO) :-
    io.lock_stream_db(!IO),
    io.get_stream_db(StreamDb0, !IO),
    map.set(get_stream_id(Stream), Name, StreamDb0, StreamDb),
    io.set_stream_db(StreamDb, !IO),
    io.unlock_stream_db(!IO).

:- pred io.maybe_delete_stream_info(io.stream::in, io::di, io::uo) is det.

io.maybe_delete_stream_info(Stream, !IO) :-
    io.may_delete_stream_info(MayDeleteStreamInfo, !IO),
    ( MayDeleteStreamInfo \= 0 ->
        io.lock_stream_db(!IO),
        io.get_stream_db(StreamDb0, !IO),
        map.delete(get_stream_id(Stream), StreamDb0, StreamDb),
        io.set_stream_db(StreamDb, !IO),
        io.unlock_stream_db(!IO)
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
    io.may_delete_stream_info(MayDelete::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    MayDelete = !MR_debug_ever_enabled;
").

io.may_delete_stream_info(1, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Global state predicates
%

io.get_globals(Globals, !IO) :-
    io.lock_globals(!IO),
    io.unsafe_get_globals(Globals, !IO),
    io.unlock_globals(!IO).

io.set_globals(Globals, !IO) :-
    io.lock_globals(!IO),
    io.unsafe_set_globals(Globals, !IO),
    io.unlock_globals(!IO).

:- pragma promise_pure(io.update_globals/3).

io.update_globals(UpdatePred, !IO) :-
    io.lock_globals(!IO),
    io.unsafe_get_globals(Globals0, !IO),
    promise_equivalent_solutions [!:IO] (
        Update = (pred(G::out) is det :-
            UpdatePred(Globals0, G)
        ),
        try(Update, UpdateResult),
        (
            UpdateResult = succeeded(Globals),
            io.unsafe_set_globals(Globals, !IO),
            io.unlock_globals(!IO)
        ;
            % If the update operation threw an exception
            % then release the lock and rethrow the exception.
            UpdateResult = exception(_),
            impure io.unlock_globals,
            rethrow(UpdateResult)
        )
    ).

:- pred io.lock_globals(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.lock_globals(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    #ifdef MR_THREAD_SAFE
        MR_LOCK(&ML_io_user_globals_lock, \"io.lock_globals/2\");
    #endif
").

    % For the non-C backends.
    %
lock_globals(!IO).

:- pred io.unlock_globals(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.unlock_globals(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    #ifdef MR_THREAD_SAFE
        MR_UNLOCK(&ML_io_user_globals_lock, \"io.unlock_globals/2\");
    #endif
").

    % For the non-C backends.
    %
unlock_globals(!IO).

:- impure pred io.unlock_globals is det.
:- pragma foreign_proc("C",
    io.unlock_globals,
    [will_not_call_mercury, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    #ifdef MR_THREAD_SAFE
        MR_UNLOCK(&ML_io_user_globals_lock, \"io.unlock_globals/2\");
    #endif
").
    % For the non-C backends.
    %
io.unlock_globals :-
    impure impure_true.

    % NOTE: io.unsafe_{get, set}_globals/3 are marked as `thread_safe' so that
    % calling them to does not acquire the global lock.  Since calls to these
    % predicates should be surrounded by calls to io.{lock, unlock}_globals/2
    % this is safe.
    %
:- pred io.unsafe_get_globals(univ::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.unsafe_get_globals(Globals::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    Globals = ML_io_user_globals;
").

:- pred io.unsafe_set_globals(univ::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.unsafe_set_globals(Globals::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /* XXX need to globalize the memory */
    ML_io_user_globals = Globals;
").

:- pragma foreign_proc("C#",
    io.unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = io.ML_io_user_globals;
").

:- pragma foreign_proc("C#",
    io.unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_user_globals = Globals;
").

:- pragma foreign_proc("Java",
    io.unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Globals = io.ML_io_user_globals;
").

:- pragma foreign_proc("Java",
    io.unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    io.ML_io_user_globals = Globals;
").

% XXX the following Erlang implementation doesn't work with multiple threads

:- pragma foreign_proc("Erlang",
    io.unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = get('ML_io_user_globals')
").

:- pragma foreign_proc("Erlang",
    io.unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    put('ML_io_user_globals', Globals)
").

%-----------------------------------------------------------------------------%

io.progname_base(DefaultName, PrognameBase, !IO) :-
    io.progname(DefaultName, Progname, !IO),
    PrognameBase = dir.det_basename(Progname).

:- pragma foreign_proc("C",
    io.get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure, does_not_affect_liveness,
        no_sharing],
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
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    Id = Stream.id;
").

:- pragma foreign_proc("Erlang",
    io.get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    {'ML_stream', Id, _IoDevice} = Stream
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Environment interface predicates

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
%
% Miscellaneous predicates
%

:- interface.

    % XXX Since on the IL backend pragma export is NYI, this
    % predicate must be placed in the interface.
    %
:- pred io.init_state(io::di, io::uo) is det.

:- implementation.

    % For use by the Mercury runtime.
    %
:- pragma foreign_export("C", io.init_state(di, uo), "ML_io_init_state").
:- pragma foreign_export("IL", io.init_state(di, uo), "ML_io_init_state").
:- pragma foreign_export("Erlang", io.init_state(di, uo), "ML_io_init_state").

io.init_state(!IO) :-
    init_std_streams(!IO),
    %
    % In C grades the "current" streams are thread-local values, so can only be
    % set after the MR_Context has been initialised for the initial thread.
    %
    io.set_input_stream(io.stdin_stream, _, !IO),
    io.set_output_stream(io.stdout_stream, _, !IO),
    io.stdin_binary_stream(StdinBinary, !IO),
    io.stdout_binary_stream(StdoutBinary, !IO),
    io.set_binary_input_stream(StdinBinary, _, !IO),
    io.set_binary_output_stream(StdoutBinary, _, !IO),

    io.gc_init(type_of(StreamDb), type_of(Globals), !IO),
    map.init(StreamDb),
    type_to_univ("<globals>", Globals),
    io.set_stream_db(StreamDb, !IO),
    io.set_op_table(ops.init_mercury_op_table, !IO),
    io.set_globals(Globals, !IO),
    io.insert_std_stream_names(!IO).

:- pred init_std_streams(io::di, io::uo) is det.

init_std_streams(!IO).

:- pragma foreign_proc("Erlang",
    init_std_streams(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    F = (fun() -> mercury_stdio_file_server(group_leader()) end),
    StdinPid = spawn(F),
    StdoutPid = spawn(F),
    StderrPid = spawn(F),
    StdinBinaryPid = spawn(F),
    StdoutBinaryPid = spawn(F),

    Stdin = {'ML_stream', make_ref(), StdinPid},
    Stdout = {'ML_stream', make_ref(), StdoutPid},
    Stderr = {'ML_stream', make_ref(), StderrPid},
    StdinBinary = {'ML_stream', make_ref(), StdinBinaryPid},
    StdoutBinary = {'ML_stream', make_ref(), StdoutBinaryPid},

    % Initialise the process dictionary.
    put('ML_stdin_stream', Stdin),
    put('ML_stdout_stream', Stdout),
    put('ML_stderr_stream', Stderr),
    put('ML_stdin_binary_stream', StdinBinary),
    put('ML_stdout_binary_stream', StdoutBinary),

    % Save the standard streams to the global server. When we spawn a new
    % Mercury thread later we will need to look it up in order to initialise
    % the new process's process dictionary.
    StdStreams = {Stdin, Stdout, Stderr, StdinBinary, StdoutBinary},
    'ML_erlang_global_server' ! {init_std_streams, StdStreams}
").

:- pred io.finalize_state(io::di, io::uo) is det.

    % For use by the Mercury runtime.
    %
:- pragma foreign_export("C", io.finalize_state(di, uo),
    "ML_io_finalize_state").
:- pragma foreign_export("IL", io.finalize_state(di, uo),
    "ML_io_finalize_state").
:- pragma foreign_export("Erlang", io.finalize_state(di, uo),
    "ML_io_finalize_state").

    % Currently no finalization needed...
    % (Perhaps we should close all open Mercury files?
    % That will happen on process exit anyway, so currently we don't bother.)
io.finalize_state(!IO).

:- pred io.gc_init(type_desc::in, type_desc::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.gc_init(StreamDbType::in, UserGlobalsType::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    /* for Windows DLLs, we need to call GC_INIT() from each DLL */
#ifdef MR_BOEHM_GC
    GC_INIT();
#endif
    MR_add_root(&ML_io_stream_db, (MR_TypeInfo) StreamDbType);
    MR_add_root(&ML_io_user_globals, (MR_TypeInfo) UserGlobalsType);
").

io.gc_init(_, _, !IO).

:- pred io.insert_std_stream_names(io::di, io::uo) is det.

io.insert_std_stream_names(!IO) :-
    io.stdin_stream(input_stream(Stdin), !IO),
    io.insert_stream_info(Stdin, stream(0, input, preopen, stdin), !IO),
    io.stdout_stream(output_stream(Stdout), !IO),
    io.insert_stream_info(Stdout, stream(1, output, preopen, stdout), !IO),
    io.stderr_stream(output_stream(Stderr), !IO),
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
    % exception handler, does not print out the module name.

io.make_io_error(Error) = io_error(Error).

io.error_message(Error) = Msg :-
    io.error_message(Error, Msg).

io.error_message(io_error(Error), Error).

%-----------------------------------------------------------------------------%

    % XXX design flaw with regard to unique modes and
    % io.get_op_table

io.get_op_table(ops.init_mercury_op_table, !IO).

io.set_op_table(_OpTable, !IO).

%-----------------------------------------------------------------------------%

% For use by the debugger:

:- pred io.get_io_input_stream_type(type_desc::out, io::di, io::uo) is det.

:- pragma foreign_export("C", io.get_io_input_stream_type(out, di, uo),
    "ML_io_input_stream_type").
:- pragma foreign_export("IL", io.get_io_input_stream_type(out, di, uo),
    "ML_io_input_stream_type").

io.get_io_input_stream_type(Type, !IO) :-
    io.stdin_stream(Stream, !IO),
    Type = type_of(Stream).

:- pred io.get_io_output_stream_type(type_desc::out, io::di, io::uo) is det.

:- pragma foreign_export("C", io.get_io_output_stream_type(out, di, uo),
    "ML_io_output_stream_type").
:- pragma foreign_export("IL", io.get_io_output_stream_type(out, di, uo),
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

#ifdef MR_WIN32
  #include ""mercury_windows.h""
#endif

#if defined(MR_MSVC)
    typedef SSIZE_T ML_ssize_t;
#else
    typedef ssize_t ML_ssize_t;
#endif

extern MercuryFile mercury_stdin;
extern MercuryFile mercury_stdout;
extern MercuryFile mercury_stderr;
extern MercuryFile mercury_stdin_binary;
extern MercuryFile mercury_stdout_binary;
extern MR_Unsigned mercury_current_text_input_index;
extern MR_Unsigned mercury_current_text_output_index;
extern MR_Unsigned mercury_current_binary_input_index;
extern MR_Unsigned mercury_current_binary_output_index;

#define MR_initial_io_state()       0   /* some random number */
#define MR_final_io_state(r)        ((void)0)

void            mercury_init_io(void);
MercuryFilePtr  mercury_current_text_input(void);
MercuryFilePtr  mercury_current_text_output(void);
MercuryFilePtr  mercury_current_binary_input(void);
MercuryFilePtr  mercury_current_binary_output(void);
int             mercury_next_stream_id(void);
MercuryFilePtr  mercury_open(const char *filename, const char *openmode,
                    MR_AllocSiteInfoPtr alloc_id);
void            mercury_io_error(MercuryFilePtr mf, const char *format, ...);
void            mercury_output_error(MercuryFilePtr mf);
void            mercury_print_string(MercuryFilePtr mf, const char *s);
int             mercury_get_byte(MercuryFilePtr mf);
void            mercury_close(MercuryFilePtr mf);
int             ML_fprintf(MercuryFilePtr mf, const char *format, ...);

#ifdef MR_WIN32
    wchar_t     *ML_utf8_to_wide(const char *s);
    char        *ML_wide_to_utf8(const wchar_t *ws,
                    MR_AllocSiteInfoPtr alloc_id);
#endif
").

:- pragma foreign_code("C#", "

    public enum ML_line_ending_kind {
        ML_OS_line_ending,      // file uses the usual line-ending convention
                                // for the OS (e.g. CR-LF for DOS/Windows).

        ML_Unix_line_ending,    // file uses the Unix line-encoding convention.

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
        public System.IO.TextWriter writer; // The stream writer for it
        public int                  putback;
                                    // the next character or byte to read,
                                    // or -1 if no putback char/byte is stored

        public ML_line_ending_kind  line_ending;
                                    // DOS, Unix, or raw binary

        public int                  line_number;
        public int                  id;
    };
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
    ** FileInput/OutputStreams and seeking is controlled through use of
    ** FileChannels (requring Java versions >= 1.4).
    **
    ** The use of the methods in this implementation is not very flexible.
    ** You may not perform text mode operations on a binary file or vice
    ** versa.
    ** XXX This causes problems for io.read_binary and io.write_binary,
    ** for which the current implementations attempt to access binary
    ** streams using text mode operations (and so will definitely break.)
    */

    public abstract static class MR_MercuryFileStruct {
        public  static int                  ML_next_stream_id = 0;
        public  int                         id;

        protected MR_MercuryFileStruct() {
            assign_id();
        }

        private synchronized void assign_id() {
            id = ML_next_stream_id++;
        }

        abstract public void close() throws java.io.IOException;
    }

    public static class MR_TextInputFile
        extends MR_MercuryFileStruct
    {
        private java.io.InputStreamReader   input       = null;
        private char[]                      buf         = new char[1024];
        private int                         buf_pos     = 0;
        private int                         buf_end     = 0;
        public  int                         line_number = 1;

        public MR_TextInputFile(java.io.InputStream stream) {
            try {
                input = new java.io.InputStreamReader(stream, ""UTF-8"");
            } catch (java.io.UnsupportedEncodingException e) {
                // This will never happen.
                throw new Error(e.toString());
            }
        }

        /*
        ** refill_buffer():
        ** Returns true if the end of the stream has not been reached.
        */
        private boolean refill_buffer()
            throws java.io.IOException
        {
            if (buf_end == buf_pos) {
                int n = input.read(buf, 0, buf.length);
                if (n == -1) {
                    return false;
                }
                buf_end = n;
                buf_pos = 0;
            }
            return true;
        }

        private int read_code_unit()
            throws java.io.IOException
        {
            if (!refill_buffer()) {
                return -1;
            }

            char c = buf[buf_pos];
            buf_pos++;
            return (int) c;
        }

        /*
        ** read_char(): [Java]
        **
        ** Reads one character in from a text input file using the default
        ** charset decoding.  Returns -1 at end of file.
        */
        public int read_char()
            throws java.io.IOException
        {
            final int c1 = read_code_unit();
            if (c1 == -1) {
                return -1;
            }
            if (c1 == '\\n') {
                line_number++;
            }
            if (!Character.isHighSurrogate((char) c1)) {
                return c1;
            }
            final int c2 = read_code_unit();
            if (c2 != -1 && !Character.isLowSurrogate((char) c2)) {
                // Return replacement character.
                return 0xfffd;
            }
            return Character.toCodePoint((char) c1, (char) c2);
        }

        /*
        ** read_line(): [Java]
        **
        ** Reads in a line of a text input file using the default
        ** charset decoding.  Returns null at end of file.
        */
        public String read_line()
            throws java.io.IOException
        {
            if (!refill_buffer()) {
                return null;
            }

            /* Commonly, the buffer already contains a complete line. */
            for (int i = buf_pos; i < buf_end; i++) {
                if (buf[i] == '\\n') {
                    String s = new String(buf, buf_pos, i - buf_pos + 1);
                    buf_pos = i + 1;
                    line_number++;
                    return s;
                }
            }

            /* The buffer doesn't contain a complete line. */
            StringBuilder sb = new StringBuilder();

            sb.append(buf, buf_pos, buf_end - buf_pos);
            buf_pos = buf_end;

            while (refill_buffer()) {
                for (int i = buf_pos; i < buf_end; i++) {
                    if (buf[i] == '\\n') {
                        sb.append(buf, buf_pos, i - buf_pos + 1);
                        buf_pos = i + 1;
                        line_number++;
                        return sb.toString();
                    }
                }

                sb.append(buf, buf_pos, buf_end - buf_pos);
                buf_pos = buf_end;
            }

            return sb.toString();
        }

        /*
        ** read_file(): [Java]
        **
        ** Reads in the rest of a text input file using the default
        ** charset decoding.
        */
        public void read_file(StringBuilder sb)
            throws java.io.IOException
        {
            int n;

            sb.append(buf, buf_pos, buf_end - buf_pos);
            buf_pos = buf_end;

            while ((n = input.read(buf)) > -1) {
                for (int i = 0; i < n; i++) {
                    if (buf[i] == '\\n') {
                        line_number++;
                    }
                }
                sb.append(buf, 0, n);
            }
        }

        private void unget_code_unit(char c) {
            /*
            ** If necessary, shift the unread characters in the input buffer
            ** to make room at the front of the buffer.  If the buffer is full
            ** then allocate a bigger buffer.
            */
            if (buf_pos == 0) {
                if (buf_end < buf.length) {
                    int offset = buf.length - buf_end;
                    System.arraycopy(buf, 0, buf, offset, buf_end);
                    buf_pos = offset;
                    buf_end = buf.length;
                } else {
                    char[] new_buf = new char[buf.length * 2];
                    int offset = new_buf.length - buf_end;
                    System.arraycopy(buf, 0, new_buf, offset, buf_end);
                    buf = new_buf;
                    buf_pos = offset;
                    buf_end = new_buf.length;
                }
            }

            buf[--buf_pos] = c;
        }

        public void ungetc(int c) {
            if (Character.charCount(c) == 1) {
                unget_code_unit((char) c);
                if (c == '\\n') {
                    line_number--;
                }
            } else {
                char[] units = Character.toChars(c);
                unget_code_unit(units[1]);
                unget_code_unit(units[0]);
            }
        }

        @Override
        public void close()
            throws java.io.IOException
        {
            input.close();
        }
    } // class MR_TextInputFile

    public static class MR_TextOutputFile
        extends MR_MercuryFileStruct
    {
        private java.io.BufferedWriter  output      = null;
        public  int                     line_number = 1;

        public MR_TextOutputFile(java.io.OutputStream stream) {
            try {
                output = new java.io.BufferedWriter(
                    new java.io.OutputStreamWriter(stream, ""UTF-8""));
            }
            catch (java.io.UnsupportedEncodingException e) {
                // This will never happen.
                throw new Error(e.toString());
            }
        }

        public void put(char c)
            throws java.io.IOException
        {
            output.write(c);
            if (c == '\\n') {
                output.flush();
                line_number++;
            }
        }

        public void put_or_throw(char c) {
            try {
                put(c);
            } catch (java.io.IOException e) {
                io.ML_throw_io_error(e.getMessage());
            }
        }

        public void write(java.lang.String s)
            throws java.io.IOException
        {
            output.write(s);

            int old_line_number = line_number;
            for (int i = 0; i < s.length(); i++) {
                if (s.charAt(i) == '\\n') {
                    line_number++;
                }
            }

            /* Flush if we saw a newline. */
            if (old_line_number != line_number) {
                output.flush();
            }
        }

        public void write_or_throw(java.lang.String s) {
            try {
                write(s);
            } catch (java.io.IOException e) {
                io.ML_throw_io_error(e.getMessage());
            }
        }

        public void flush()
            throws java.io.IOException
        {
            output.flush();
        }

        public void flush_or_throw() {
            try {
                flush();
            } catch (java.io.IOException e) {
                io.ML_throw_io_error(e.getMessage());
            }
        }

        @Override
        public void close()
            throws java.io.IOException
        {
            output.close();
        }
    } // class MR_TextOutputFile

    public abstract static class MR_BinaryFile
        extends MR_MercuryFileStruct
    {
        // These must match whence_to_int.
        protected static final int  SEEK_SET = 0;
        protected static final int  SEEK_CUR = 1;
        protected static final int  SEEK_END = 2;

        // channel is used for positioning the stream.  Read/write operations
        // use randomaccess, binary_input, binary_output instead.
        protected java.nio.channels.FileChannel channel = null;

        /*
        ** size(): [Java]
        **
        ** Returns the length of a file.
        */
        public int size()
            throws java.io.IOException
        {
            return (int) channel.size();
        }

        /*
        ** getOffset():
        **  Returns the current position in a binary file.
        */
        abstract public int getOffset() throws java.io.IOException;

        /*
        ** seek(): [Java]
        **
        ** Seek relative to start, current position or end depending on the
        ** flag.
        */
        public void seek_binary(int flag, int offset)
            throws java.io.IOException
        {
            long position;

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

            channel.position(position);
        }
    }

    public static class MR_BinaryInputFile
        extends MR_BinaryFile
    {
        private java.io.FileInputStream     binary_input = null;
        protected java.util.Stack<Byte>     pushback
                                                = new java.util.Stack<Byte>();

        public MR_BinaryInputFile(java.io.FileInputStream in) {
            this.binary_input = in;
            this.channel = in.getChannel();
        }

        /*
        ** read_byte(): [Java]
        **
        ** Reads one byte in from a binary file. Returns -1 at end of file.
        */
        public int read_byte()
            throws java.io.IOException
        {
            int c;
            if (pushback.empty()) {
                c = binary_input.read();
            } else {
                c = pushback.pop();
            }
            return c;
        }

        public void ungetc(byte b) {
            pushback.push(b);
        }

        @Override
        public int getOffset()
            throws java.io.IOException
        {
            return (int) channel.position() - pushback.size();
        }

        @Override
        public void seek_binary(int flag, int offset)
            throws java.io.IOException
        {
            super.seek_binary(flag, offset);

            pushback = new java.util.Stack<Byte>();
        }

        @Override
        public void close()
            throws java.io.IOException
        {
            binary_input.close();
        }
    }

    public static class MR_BinaryOutputFile
        extends MR_BinaryFile
    {
        private java.io.FileOutputStream    binary_output = null;

        public MR_BinaryOutputFile(java.io.FileOutputStream out) {
            this.binary_output = out;
            this.channel = out.getChannel();
        }

        public void put(byte b)
            throws java.io.IOException
        {
            binary_output.write(b);
        }

        public void put_or_throw(byte b) {
            try {
                put(b);
            } catch (java.io.IOException e) {
                io.ML_throw_io_error(e.getMessage());
            }
        }

        // Obsolete.
        public void write(java.lang.String s)
            throws java.io.IOException
        {
            for (int i = 0; i < s.length(); i++) {
                // lower 8 bits of each
                put((byte) s.charAt(i));
            }
        }

        public void write_or_throw(java.lang.String s) {
            try {
                write(s);
            } catch (java.io.IOException e) {
                io.ML_throw_io_error(e.getMessage());
            }
        }

        public void flush()
            throws java.io.IOException
        {
            binary_output.flush();
        }

        public void flush_or_throw() {
            try {
                flush();
            } catch (java.io.IOException e) {
                io.ML_throw_io_error(e.getMessage());
            }
        }

        @Override
        public int getOffset()
            throws java.io.IOException
        {
            return (int) channel.position();
        }

        @Override
        public void close()
            throws java.io.IOException
        {
            binary_output.close();
        }
    }

    // StreamPipe is a mechanism for connecting streams to those of a
    // Runtime.exec() Process.

    private static class StreamPipe extends java.lang.Thread {
        MR_TextInputFile        in;
        MR_TextOutputFile       out;
        boolean                 closeOutput = false;
        java.lang.Exception     exception = null;

        StreamPipe(java.io.InputStream in, MR_TextOutputFile out) {
            this.in  = new MR_TextInputFile(in);
            this.out = out;
        }

        StreamPipe(MR_TextInputFile in, java.io.OutputStream out) {
            this.in  = in;
            this.out = new MR_TextOutputFile(out);
            closeOutput = true;
        }

        public void run() {
            try {
                while (true) {
                    int c = in.read_char();
                    if (c == -1 || interrupted()) {
                        break;
                    }
                    out.put((char) c);
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
").

:- pragma foreign_code("C", "

MercuryFile mercury_stdin;
MercuryFile mercury_stdout;
MercuryFile mercury_stderr;
MercuryFile mercury_stdin_binary;
MercuryFile mercury_stdout_binary;

MR_Unsigned mercury_current_text_input_index;
MR_Unsigned mercury_current_text_output_index;
MR_Unsigned mercury_current_binary_input_index;
MR_Unsigned mercury_current_binary_output_index;

static void
mercury_set_binary_mode(FILE *f)
{
#if defined(MR_MSVC) || defined(MR_MINGW)
    /*
    ** Calling fdopen with 'b' in the mode string does not necessarily put the
    ** standard input or standard output file into binary translation mode on
    ** Windows. This is the case with MinGW and MSVC. The cause is likely the
    ** MSVC CRT. The solution is to change the mode on the file after opening.
    */
    _setmode(_fileno(f), _O_BINARY);
#endif
}

void
mercury_init_io(void)
{
    MR_mercuryfile_init(stdin, 1, &mercury_stdin);
    MR_mercuryfile_init(stdout, 1, &mercury_stdout);
    MR_mercuryfile_init(stderr, 1, &mercury_stderr);

    MR_mercuryfile_init(NULL, 1, &mercury_stdin_binary);
    MR_mercuryfile_init(NULL, 1, &mercury_stdout_binary);

    mercury_current_text_input_index = MR_new_thread_local_mutable_index();
    mercury_current_text_output_index = MR_new_thread_local_mutable_index();
    mercury_current_binary_input_index = MR_new_thread_local_mutable_index();
    mercury_current_binary_output_index = MR_new_thread_local_mutable_index();

#if defined(MR_HAVE_FDOPEN) && (defined(MR_HAVE_FILENO) || defined(fileno)) && \
        defined(MR_HAVE_DUP)
    MR_file(mercury_stdin_binary) = fdopen(dup(fileno(stdin)), ""rb"");
    if (MR_file(mercury_stdin_binary) != NULL) {
        mercury_set_binary_mode(MR_file(mercury_stdin_binary));
    } else {
        /*
        ** The call to fdopen() may fail if stdin is not available.
        ** We don't abort since we still want Mercury programs to be runnable
        ** in such a circumstance (aside from those that use stdin).
        ** For the same reason we treat binary stdout identically below.
        **
        ** NOTE: some versions of nohup may also cause the above call to
        **       fdopen() to fail because they redirect stdin to /dev/null
        **       in *write* mode.  Setting binary stdin to stdin in such
        **       a case also ensures that we work with those versions of
        **       nohup.
        */
        MR_file(mercury_stdin_binary) = stdin;
    }

    MR_file(mercury_stdout_binary) = fdopen(dup(fileno(stdout)), ""wb"");
    if (MR_file(mercury_stdout_binary) != NULL) {
        mercury_set_binary_mode(MR_file(mercury_stdout_binary));
    } else {
        MR_file(mercury_stdout_binary) = stdout;
    }
#else
    /*
    ** XXX Standard ANSI/ISO C provides no way to set stdin/stdout
    ** to binary mode.  I guess we just have to punt...
    */
    MR_file(mercury_stdin_binary) = stdin;
    MR_file(mercury_stdout_binary) = stdout;
#endif

#ifdef MR_THREAD_SAFE
    pthread_mutex_init(&ML_io_stream_db_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&ML_io_user_globals_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&ML_io_next_stream_id_lock, MR_MUTEX_ATTR);
#endif
}

MercuryFilePtr
mercury_current_text_input(void)
{
    MercuryFilePtr stream;
    MR_get_thread_local_mutable(MercuryFilePtr, stream,
        mercury_current_text_input_index);
    return stream;
}

MercuryFilePtr
mercury_current_text_output(void)
{
    MercuryFilePtr stream;
    MR_get_thread_local_mutable(MercuryFilePtr, stream,
        mercury_current_text_output_index);
    return stream;
}

MercuryFilePtr
mercury_current_binary_input(void)
{
    MercuryFilePtr stream;
    MR_get_thread_local_mutable(MercuryFilePtr, stream,
        mercury_current_binary_input_index);
    return stream;
}

MercuryFilePtr
mercury_current_binary_output(void)
{
    MercuryFilePtr stream;
    MR_get_thread_local_mutable(MercuryFilePtr, stream,
        mercury_current_binary_output_index);
    return stream;
}

int
mercury_next_stream_id(void)
{
    int id;
    MR_LOCK(&ML_io_next_stream_id_lock, ""io.do_open_text"");
    id = ML_next_stream_id++;
    MR_UNLOCK(&ML_io_next_stream_id_lock, ""io.do_open_text"");
    return id;
}
").

:- pragma foreign_code("C#", "

static MR_MercuryFileStruct
mercury_file_init(System.IO.Stream stream,
    System.IO.TextReader reader, System.IO.TextWriter writer,
    ML_line_ending_kind line_ending)
{
    MR_MercuryFileStruct mf = new MR_MercuryFileStruct();
    mf.stream = stream;
    mf.reader = reader;
    mf.putback = -1;
    mf.writer = writer;
    mf.line_ending = line_ending;
    mf.line_number = 1;
    mf.id = ML_next_stream_id++;
    return mf;
}

    // Note: for Windows GUI programs, the Console is set to the equivalent
    // of /dev/null.  This could perhaps be considered a problem. But if so,
    // it is a problem in Windows, not in Mercury -- I don't think it is one
    // that the Mercury implementation should try to solve.

public static MR_MercuryFileStruct mercury_stdin =
    mercury_file_init(System.Console.OpenStandardInput(),
        System.Console.In, null, ML_default_line_ending);
public static MR_MercuryFileStruct mercury_stdout =
    mercury_file_init(System.Console.OpenStandardOutput(),
        null, System.Console.Out, ML_default_line_ending);
public static MR_MercuryFileStruct mercury_stderr =
    mercury_file_init(System.Console.OpenStandardError(),
        null, System.Console.Error, ML_default_line_ending);

    // XXX should we use BufferedStreams here?
public static MR_MercuryFileStruct mercury_stdin_binary =
    mercury_file_init(System.Console.OpenStandardInput(),
        System.Console.In, null, ML_line_ending_kind.ML_raw_binary);
public static MR_MercuryFileStruct mercury_stdout_binary =
    mercury_file_init(System.Console.OpenStandardOutput(),
        null, System.Console.Out, ML_line_ending_kind.ML_raw_binary);

// Note: these are set again in io.init_state.
public static MR_MercuryFileStruct mercury_current_text_input =
    mercury_stdin;
public static MR_MercuryFileStruct mercury_current_text_output =
    mercury_stdout;
public static MR_MercuryFileStruct mercury_current_binary_input =
    mercury_stdin_binary;
public static MR_MercuryFileStruct mercury_current_binary_output =
    mercury_stdout_binary;

// XXX not thread-safe!
public static System.Exception MR_io_exception;

").

:- pragma foreign_code("Java",
"
public static MR_TextInputFile mercury_stdin =
    new MR_TextInputFile(java.lang.System.in);

public static MR_TextOutputFile mercury_stdout =
    new MR_TextOutputFile(java.lang.System.out);

public static MR_TextOutputFile mercury_stderr =
    new MR_TextOutputFile(java.lang.System.err);

/**
 * We initialize mercury_stdin_binary and mercury_stdout_binary
 * only when they are needed,  because the initialization code
 * does not work on Google's App Engine.
 */
private static MR_BinaryInputFile mercury_stdin_binary = null;

private static MR_BinaryOutputFile mercury_stdout_binary = null;

private static void ensure_init_mercury_stdin_binary() {
    if (mercury_stdin_binary == null) {
        mercury_stdin_binary = new MR_BinaryInputFile(
            new java.io.FileInputStream(java.io.FileDescriptor.in));
    }
}

private static void ensure_init_mercury_stdout_binary() {
    if (mercury_stdout_binary == null) {
        mercury_stdout_binary = new MR_BinaryOutputFile(
            new java.io.FileOutputStream(java.io.FileDescriptor.out));
    }
}

// Note: these are also set in io.init_state.

public static ThreadLocal<MR_TextInputFile> mercury_current_text_input =
    new InheritableThreadLocal<MR_TextInputFile>() {
        protected MR_TextInputFile initialValue() {
            return mercury_stdin;
        }
    };

public static ThreadLocal<MR_TextOutputFile> mercury_current_text_output =
    new InheritableThreadLocal<MR_TextOutputFile>() {
        protected MR_TextOutputFile initialValue() {
            return mercury_stdout;
        }
    };

public static ThreadLocal<MR_BinaryInputFile> mercury_current_binary_input =
    new InheritableThreadLocal<MR_BinaryInputFile>() {
        protected MR_BinaryInputFile initialValue() {
            ensure_init_mercury_stdin_binary();
            return mercury_stdin_binary;
        }
    };

public static ThreadLocal<MR_BinaryOutputFile> mercury_current_binary_output =
    new InheritableThreadLocal<MR_BinaryOutputFile>() {
        protected MR_BinaryOutputFile initialValue() {
            ensure_init_mercury_stdout_binary();
            return mercury_stdout_binary;
        }
    };

public static ThreadLocal<Exception> MR_io_exception =
    new ThreadLocal<Exception>();
").

:- pragma foreign_decl("Erlang", local, "

    % These need to be exported because code in foreign_procs may be inlined
    % into other modules.  Hence, calls to these functions must be module
    % qualified as well.
    %
-export([
    mercury_open_stream/2,
    mercury_close_stream/1,
    mercury_getc/1,
    mercury_read_string_to_eof/1,
    mercury_putback/2,
    mercury_write_string/2,
    mercury_write_char/2,
    mercury_write_int/2,
    mercury_sync/1,
    mercury_get_line_number/1,
    mercury_set_line_number/2,
    mercury_get_pos/1,
    mercury_set_pos/2,
    mercury_get_file_size/1,

    % We may want to inline the following by hand to avoid inter-module calls.
    mercury_set_current_text_input/1,
    mercury_set_current_text_output/1,
    mercury_set_current_binary_input/1,
    mercury_set_current_binary_output/1
]).
").

:- pragma foreign_decl("Erlang", "

    % Avoid an intermodule function call every time we want to get the current
    % stream.
    %
-define(ML_get_current_text_input, get('ML_io_current_text_input')).
-define(ML_get_current_text_output, get('ML_io_current_text_output')).
-define(ML_get_current_binary_input, get('ML_io_current_binary_input')).
-define(ML_get_current_binary_output, get('ML_io_current_binary_output')).
").

:- pragma foreign_code("Erlang", "

    % For each open file we have a process running in the background.
    % This is necessary so we can layer pushback support and line number
    % tracking on top of what the Erlang runtime provides.
    %
    % Note that we send back acknowledgements for all messages.  This is to
    % ensure that two operations from the same process are done in order.
    %
mercury_start_file_server(ParentPid, FileName, Mode) ->
    Encoding = {encoding, utf8},
    case Mode of
        [$r | _] ->
            ModeList = [read, read_ahead, binary, Encoding];
        [$w | _] ->
            ModeList = [write, delayed_write, binary, Encoding];
        [$a | _] ->
            ModeList = [append, delayed_write, binary, Encoding]
    end,
    case file:open(FileName, ModeList) of
        {ok, IoDevice} ->
            StreamId = make_ref(),
            Stream = {'ML_stream', StreamId, self()},
            ParentPid ! {self(), open_ack, {ok, Stream}},
            mercury_file_server(IoDevice, 1, [])
    ;
        {error, Reason} ->
            ParentPid ! {self(), open_ack, {error, Reason}}
    end.

mercury_stdio_file_server(IoDevice) ->
    io:setopts(IoDevice, [binary, {encoding, utf8}]),
    mercury_file_server(IoDevice, 1, []).

mercury_file_server(IoDevice, LineNr0, PutBack0) ->
    receive
        {From, close} ->
            From ! {self(), close_ack},
            file:close(IoDevice)
            % XXX check error?
    ;
        {From, read_char} ->
            case PutBack0 of
                [] ->
                    Prompt = '',
                    GetChars = io:get_chars(IoDevice, Prompt, 1),
                    case GetChars of
                        <<Char/utf8>> ->
                            Ret = Char,
                            LineNr = LineNr0 + one_if_nl(Char);
                        EofOrError ->
                            Ret = EofOrError,
                            LineNr = LineNr0
                    end,
                    PutBack = PutBack0;
                [Char | PutBack] ->
                    Ret = Char,
                    LineNr = LineNr0 + one_if_nl(Char)
            end,
            From ! {self(), read_char_ack, Ret},
            mercury_file_server(IoDevice, LineNr, PutBack)
    ;
        {From, read_string_to_eof} ->
            % Grab everything from the putback buffer.
            Pre = list_to_binary(lists:reverse(PutBack0)),
            PutBack = [],

            % Read everything to EOF.
            case mercury_read_file_to_eof_2(IoDevice, []) of
                {ok, Chunks} ->
                    Binary = list_to_binary([Pre | Chunks]),
                    Ret = {ok, Binary},
                    LineNr = LineNr0 + count_nls(Binary, 0);
                {error, Chunks, Reason} ->
                    Binary = list_to_binary([Pre | Chunks]),
                    Ret = {error, list_to_binary(Chunks), Reason},
                    LineNr = LineNr0 + count_nls(Binary, 0)
            end,

            From ! {self(), read_string_to_eof_ack, Ret},
            mercury_file_server(IoDevice, LineNr, PutBack)
    ;
        {From, putback, Char} ->
            From ! {self(), putback_ack},
            PutBack = [Char | PutBack0],
            LineNr = LineNr0 - one_if_nl(Char),
            mercury_file_server(IoDevice, LineNr, PutBack)
    ;
        {From, write_char, Char} ->
            From ! {self(), write_char_ack},
            % XXX return error code
            io:put_chars(IoDevice, [Char]),
            LineNr = LineNr0 + one_if_nl(Char),
            mercury_file_server(IoDevice, LineNr, PutBack0)
    ;
        {From, write_string, Chars} ->
            From ! {self(), write_string_ack},
            % XXX return error code
            io:put_chars(IoDevice, Chars),
            LineNr = LineNr0 + count_nls(Chars, 0),
            mercury_file_server(IoDevice, LineNr, PutBack0)
    ;
        {From, write_int, Val} ->
            From ! {self(), write_int_ack},
            % XXX return error code
            io:put_chars(IoDevice, integer_to_list(Val)),
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    ;
        {From, sync} ->
            % XXX file:sync seems to hang if run on a pid, e.g. standard I/O
            if
                is_pid(IoDevice) ->
                    void;
                true ->
                    file:sync(IoDevice)
            end,
            From ! {self(), sync_ack},
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    ;
        {From, get_line_number} ->
            From ! {self(), get_line_number_ack, LineNr0},
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    ;
        {From, set_line_number, N} ->
            From ! {self(), set_line_number_ack},
            mercury_file_server(IoDevice, N, PutBack0)
    ;
        {From, get_pos} ->
            case file:position(IoDevice, cur) of
                {ok, N} ->
                    Pos = {ok, N - length(PutBack0)};
                Other ->
                    Pos = Other
            end,
            From ! {self(), get_pos_ack, Pos},
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    ;
        {From, set_pos, Loc} ->
            case Loc of
                {cur, N} ->
                    AdjLoc = {cur, N - length(PutBack0)};
                _ ->
                    AdjLoc = Loc
            end,
            SeekResult = file:position(IoDevice, AdjLoc),
            From ! {self(), set_pos_ack, SeekResult},
            PutBack = [],
            mercury_file_server(IoDevice, LineNr0, PutBack)
    ;
        {From, get_file_size} ->
            case file:pid2name(IoDevice) of
                {ok, FileName} ->
                    case file:read_file_info(FileName) of
                        {ok, FileInfo} ->
                            #file_info{size = Size} = FileInfo;
                        _ ->
                            Size = -1
                    end;
                _ ->
                    Size = -1
            end,
            From ! {self(), get_file_size, Size},
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    ;
        Other ->
            io:format(""** io.m: unrecognised message ~p~n"", [Other]),
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    end.

mercury_read_file_to_eof_2(IoDevice, Acc) ->
    ChunkSize = 65536,
    case io:get_chars(IoDevice, '', ChunkSize) of
        eof ->
            {ok, lists:reverse(Acc)};
        {error, Reason} ->
            {error, lists:reverse(Acc), Reason};
        Chunk ->
            mercury_read_file_to_eof_2(IoDevice, [Chunk | Acc])
    end.

one_if_nl($\\n) -> 1;
one_if_nl(_)    -> 0.

count_nls(Bin, N) ->
    count_nls_2(Bin, size(Bin) - 1, N).

count_nls_2(_,  -1, N) -> N;
count_nls_2(Bin, I, N) ->
    case Bin of
        <<_:I/binary, $\\n, _/binary>> ->
            count_nls_2(Bin, I - 1, N + 1);
        _ ->
            count_nls_2(Bin, I - 1, N)
    end.

% Client side.

mercury_open_stream(FileName, Mode) ->
    ParentPid = self(),
    Pid = spawn(fun() ->
        % Raw streams can only be used by the process which opened it.
        mercury_start_file_server(ParentPid, FileName, Mode)
    end),
    receive
        {Pid, open_ack, Result} ->
            Result
    end.

mercury_close_stream(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), close},
    receive
        {Pid, close_ack} ->
            void
    end.

mercury_getc(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), read_char},
    receive
        {Pid, read_char_ack, Ret} ->
            case Ret of
                C when is_integer(C) ->
                    C;
                eof ->
                    -1;
                {error, Reason} ->
                    put('MR_io_exception', Reason),
                    -2
            end
    end.

mercury_read_string_to_eof(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), read_string_to_eof},
    receive
        {Pid, read_string_to_eof_ack, Ret} ->
            case Ret of
                {error, _Partial, Reason} ->
                    put('MR_io_exception', Reason);
                _ ->
                    void
            end,
            Ret
    end.

mercury_putback(Stream, Character) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), putback, Character},
    receive
        {Pid, putback_ack} ->
            % Putbacks always succeed so there is no data.
            void
    end.

mercury_write_string(Stream, Characters) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), write_string, Characters},
    receive
        {Pid, write_string_ack} ->
            void
    end.

mercury_write_char(Stream, Character) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), write_char, Character},
    receive
        {Pid, write_char_ack} ->
            void
    end.

mercury_write_int(Stream, Value) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), write_int, Value},
    receive
        {Pid, write_int_ack} ->
            void
    end.

mercury_sync(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), sync},
    receive
        {Pid, sync_ack} ->
            void
    end.

mercury_get_line_number(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), get_line_number},
    receive
        {Pid, get_line_number_ack, LineNum} ->
            LineNum
    end.

mercury_set_line_number(Stream, LineNum) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), set_line_number, LineNum},
    receive
        {Pid, set_line_number_ack} ->
            void
    end.

mercury_get_pos(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), get_pos},
    receive
        {Pid, get_pos_ack, Result} ->
            case Result of
                {ok, NewPosition} ->
                    NewPosition;
                {error, Reason} ->
                    put('MR_io_exception', Reason),
                    -1
            end
    end.

mercury_set_pos(Stream, Loc) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), set_pos, Loc},
    receive
        {Pid, set_pos_ack, Result} ->
            case Result of
                {ok, NewPosition} ->
                    NewPosition;
                {error, Reason} ->
                    put('MR_io_exception', Reason),
                    -1
            end
    end.

mercury_get_file_size(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), get_file_size},
    receive
        {Pid, get_file_size_ack, Result} ->
            case Result of
                {ok, Size} ->
                    Size;
                {error, Reason} ->
                    put('MR_io_exception', Reason),
                    -1
            end
    end.

mercury_set_current_text_input(Stream) ->
    put('ML_io_current_text_input', Stream).

mercury_set_current_text_output(Stream) ->
    put('ML_io_current_text_output', Stream).

mercury_set_current_binary_input(Stream) ->
    put('ML_io_current_binary_input', Stream).

mercury_set_current_binary_output(Stream) ->
    put('ML_io_current_binary_output', Stream).

% We also use the key 'MR_io_exception' in the process dictionary.

").

:- pragma foreign_code("C", "

MercuryFilePtr
mercury_open(const char *filename, const char *openmode,
    MR_AllocSiteInfoPtr alloc_id)
{
    MercuryFilePtr  mf;
    FILE            *f;

#ifdef MR_WIN32
    f = _wfopen(ML_utf8_to_wide(filename), ML_utf8_to_wide(openmode));
#else
    f = fopen(filename, openmode);
#endif

    if (f == NULL) {
        return NULL;
    }
    MR_incr_hp_type_msg(mf, MercuryFile, alloc_id, ""MercuryFile"");
    MR_mercuryfile_init(f, 1, mf);
    return mf;
}

").

:- pragma foreign_code("C#", "

public static
MR_MercuryFileStruct mercury_open(string filename, string openmode,
    ML_line_ending_kind line_ending)
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
            runtime.Errors.SORRY(System.String.Concat(
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
            null, null, line_ending);
    }
}

").

:- pred throw_io_error(string::in) is erroneous.
:- pragma foreign_export("C", throw_io_error(in), "ML_throw_io_error").
:- pragma foreign_export("IL", throw_io_error(in), "ML_throw_io_error").
:- pragma foreign_export("Java", throw_io_error(in), "ML_throw_io_error").

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
public static void
mercury_print_string(MR_MercuryFileStruct mf, string s)
{
    if (mf.writer == null) {
        mf.writer = new System.IO.StreamWriter(mf.stream, text_encoding);
    }

    switch (mf.line_ending) {
    case ML_line_ending_kind.ML_raw_binary:
    case ML_line_ending_kind.ML_Unix_line_ending:
        mf.writer.Write(s);
        for (int i = 0; i < s.Length; i++) {
            if (s[i] == '\\n') {
                mf.line_number++;
            }
        }
        break;
    case ML_line_ending_kind.ML_OS_line_ending:
        // We can't just use the System.TextWriter.Write(String) method,
        // since that method doesn't convert newline characters to the
        // system's newline convention (e.g. CR-LF on Windows).
        // Only the WriteLine(...) method handles those properly.
        // So we have to output each character separately.

        for (int i = 0; i < s.Length; i++) {
            if (System.Char.IsSurrogate(s[i])) {
                mf.writer.Write(s.Substring(i, 2));
                i++;
            } else if (s[i] == '\\n') {
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

int
mercury_get_byte(MercuryFilePtr mf)
{
    int c = MR_GETCH(*mf);
    if (c == '\\n') {
        MR_line_number(*mf)++;
    }
    return c;
}

").

:- pragma foreign_code("C#", "

// Read in a character. This means reading in one or more bytes,
// converting the bytes from the system's default encoding to Unicode,
// and possibly converting CR-LF to newline. Returns -1 on error or EOF.

private static readonly string NewLine = System.Environment.NewLine;

public static int
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
        mf.reader = new System.IO.StreamReader(mf.stream, text_encoding);
    }

    c = mf.reader.Read();
    switch (mf.line_ending) {
    case ML_line_ending_kind.ML_raw_binary:
    case ML_line_ending_kind.ML_Unix_line_ending:
        if (c == '\\n') {
            mf.line_number++;
        }
        break;
    case ML_line_ending_kind.ML_OS_line_ending:
        // First, check if the character we've read matches
        // System.Environment.NewLine.
        // We assume that System.Environment.NewLine is non-null
        // and that System.Environment.NewLine.Length > 0.
        if (c != io.NewLine[0]) {
            if (c == '\\n') {
                // the input file was ill-formed, e.g. it contained only raw
                // LFs rather than CR-LF. Perhaps we should throw an exception?
                // If not, we still need to treat this as a newline, and thus
                // increment the line counter.
                mf.line_number++;
            } else if (System.Char.IsSurrogate((char) c)) {
                int c2 = mf.reader.Read();
                c = System.Char.ConvertToUtf32((char) c, (char) c2);
            }
        } else /* c == NewLine[0] */ {
            switch (io.NewLine.Length) {
            case 1:
                mf.line_number++;
                c = '\\n';
                break;
            case 2:
                if (mf.reader.Peek() == io.NewLine[1]) {
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
                runtime.Errors.SORRY(
                    ""mercury_getc: Environment.NewLine.Length"" +
                    ""is neither 1 nor 2"");
                break;
            }
        }
        break;
    }
    return c;
}

public static void
mercury_ungetc(MR_MercuryFileStruct mf, int code)
{
    if (mf.putback != -1) {
        runtime.Errors.SORRY(
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
    /*
    ** On some systems attempting to close a file stream that has been
    ** previously closed will lead to a segmentation fault.  We check
    ** that we have not previously closed the file stream here so we
    ** can give the user some idea about what has happened.
    */
    if (MR_file(*mf) == NULL) { 
        mercury_io_error(mf, ""error closing file: invalid file stream"");
    }

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

public static void
mercury_write_codepoint(System.IO.TextWriter w, int c)
{
    if (c <= 0xffff) {
        w.Write((char) c);
    } else {
        w.Write(System.Char.ConvertFromUtf32(c));
    }
}

public static void
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

:- pragma foreign_code("C", "
#ifdef MR_WIN32

/*
** Accessing Unicode file names on Windows requires that we use the functions
** taking wide character strings.
*/
wchar_t *
ML_utf8_to_wide(const char *s)
{
    int     wslen;
    wchar_t *ws;

    wslen = MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0);
    if (wslen == 0) {
        MR_fatal_error(""ML_utf8_to_wide: MultiByteToWideChar failed"");
    }
    ws = MR_GC_NEW_ARRAY(wchar_t, wslen);
    if (0 == MultiByteToWideChar(CP_UTF8, 0, s, -1, ws, wslen)) {
        MR_fatal_error(""ML_utf8_to_wide: MultiByteToWideChar failed"");
    }
    return ws;
}

char *
ML_wide_to_utf8(const wchar_t *ws, MR_AllocSiteInfoPtr alloc_id)
{
    char    *s;
    int     bytes;

    bytes = WideCharToMultiByte(CP_UTF8, 0, ws, -1, NULL, 0, NULL, NULL);
    if (bytes == 0) {
        MR_fatal_error(""ML_wide_to_utf8: WideCharToMultiByte failed"");
    }
    MR_allocate_aligned_string_msg(s, bytes, alloc_id);
    if (0 == WideCharToMultiByte(CP_UTF8, 0, ws, -1, s, bytes, NULL, NULL)) {
        MR_fatal_error(""ML_wide_to_utf8: WideCharToMultiByte failed"");
    }
    return s;
}

#endif /* MR_WIN32 */
").

%----------------------------------------------------------------------------%
%
% Input predicates
%

io.read_char_code(input_stream(Stream), CharCode, !IO) :-
    io.read_char_code_2(Stream, CharCode, !IO).

:- pred io.read_char_code_2(io.stream::in, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    io.read_char_code_2(Stream::in, CharCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing, may_not_duplicate],
"
    char    buf[5];
    int     nbytes;
    int     i;
    int     c;
    unsigned int    uc;

    c = mercury_get_byte(Stream);
    uc = c;
    if (uc <= 0x7f) {
        CharCode = uc;
    } else if (c == EOF) {
        CharCode = -1;
    } else {
        if ((uc & 0xE0) == 0xC0) {
            nbytes = 2;
        } else if ((uc & 0xF0) == 0xE0) {
            nbytes = 3;
        } else if ((uc & 0xF8) == 0xF0) {
            nbytes = 4;
        } else {
            nbytes = 0;
        }
        if (nbytes > 0) {
            buf[0] = uc;
            for (i = 1; i < nbytes; i++) {
                uc = mercury_get_byte(Stream);
                buf[i] = uc;
            }
            buf[i] = '\\0';
            CharCode = MR_utf8_get(buf, 0);
            if (CharCode < 0) {
                /* Invalid byte sequence. */
                errno = EILSEQ;
                CharCode = -2;
            }
        } else {
            /* Invalid byte sequence. */
            errno = EILSEQ;
            CharCode = -2;
        }
    }
").

io.read_byte_val(input_stream(Stream), ByteVal, !IO) :-
    io.read_byte_val_2(Stream, ByteVal, !IO).

:- pred io.read_byte_val_2(io.stream::in, int::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    io.read_byte_val_2(Stream::in, ByteVal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ByteVal = mercury_get_byte(Stream);
").

io.putback_char(input_stream(Stream), Character, !IO) :-
    io.putback_char_2(Stream, Character, !IO).

:- pred io.putback_char_2(io.stream::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.putback_char_2(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, terminates,
        does_not_affect_liveness, no_sharing, may_not_duplicate],
"
    MercuryFilePtr mf = Stream;
    if (Character == '\\n') {
        MR_line_number(*mf)--;
    }
    if (Character <= 0x7f) {
        if (MR_UNGETCH(*mf, Character) == EOF) {
            mercury_io_error(mf, ""io.putback_char: ungetc failed"");
        }
    } else {
        /* This requires multiple pushback in the underlying C library. */
        char        buf[5];
        ML_ssize_t  len;
        len = MR_utf8_encode(buf, Character);
        for (; len > 0; len--) {
            if (MR_UNGETCH(*mf, buf[len - 1]) == EOF) {
                mercury_io_error(mf, ""io.putback_char: ungetc failed"");
            }
        }
    }
").

io.putback_byte(binary_input_stream(Stream), Character, !IO) :-
    io.putback_byte_2(Stream, Character, !IO).

:- pred io.putback_byte_2(io.stream::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.putback_byte_2(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, terminates,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr mf = Stream;
    if (MR_UNGETCH(*mf, Character) == EOF) {
        mercury_io_error(mf, ""io.putback_byte: ungetc failed"");
    }
").

:- pragma foreign_proc("C#",
    io.read_char_code_2(File::in, CharCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    CharCode = io.mercury_getc(mf);
").

:- pragma foreign_proc("C#",
    io.read_byte_val_2(File::in, ByteVal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback != -1) {
        ByteVal = mf.putback;
        mf.putback = -1;
    } else {
        ByteVal = mf.stream.ReadByte();
    }
").

:- pragma foreign_proc("C#",
    io.putback_char_2(File::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"{
    io.MR_MercuryFileStruct mf = File;
    io.mercury_ungetc(mf, Character);
}").

:- pragma foreign_proc("C#",
    io.putback_byte_2(File::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"{
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback != -1) {
        runtime.Errors.SORRY(
            ""io.putback_byte: max one character of putback"");
    }
    mf.putback = Byte;
}").

:- pragma foreign_proc("Java",
    io.read_char_code_2(File::in, CharCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        CharCode = ((io.MR_TextInputFile) File).read_char();
    } catch (java.io.IOException e) {
        io.MR_io_exception.set(e);
        CharCode = -2;
    }
").

:- pragma foreign_proc("Java",
    io.read_byte_val_2(File::in, ByteVal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ByteVal = ((io.MR_BinaryInputFile) File).read_byte();
    } catch (java.io.IOException e) {
        io.MR_io_exception.set(e);
        ByteVal = -2;
    }
").

:- pragma foreign_proc("Java",
    io.putback_char_2(File::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, terminates, tabled_for_io],
"
    ((io.MR_TextInputFile) File).ungetc(Character);
").

:- pragma foreign_proc("Java",
    io.putback_byte_2(File::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, terminates, tabled_for_io],
"
    ((io.MR_BinaryInputFile) File).ungetc((byte) Byte);
").

:- pragma foreign_proc("Erlang",
    io.read_char_code_2(Stream::in, CharCode::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
    CharCode = mercury__io:mercury_getc(Stream)
").

:- pragma foreign_proc("Erlang",
    io.read_byte_val_2(Stream::in, ByteVal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
    ByteVal = mercury__io:mercury_getc(Stream)
").

:- pragma foreign_proc("Erlang",
    io.putback_char_2(File::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"
    mercury__io:mercury_putback(File, Character)
").

:- pragma foreign_proc("Erlang",
    io.putback_byte_2(File::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"
    mercury__io:mercury_putback(File, Byte)
").

%-----------------------------------------------------------------------------%
%
% Output predicates (with output to mercury_current_text_output)
%

:- pragma foreign_proc("C",
    io.write_string(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    mercury_print_string(mercury_current_text_output(), Message);
").

:- pragma foreign_proc("C",
    io.write_char(Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr out = mercury_current_text_output();
    char    buf[5];
    size_t  len;
    int     i;
    if (Character <= 0x7f) {
        if (MR_PUTCH(*out, Character) < 0) {
            mercury_output_error(out);
        }
        if (Character == '\\n') {
            MR_line_number(*out)++;
        }
    } else {
        len = MR_utf8_encode(buf, Character);
        for (i = 0; i < len; i++) {
            if (MR_PUTCH(*out, buf[i]) < 0) {
                mercury_output_error(out);
                break;
            }
        }
    }
").

:- pragma foreign_proc("C",
    io.write_int(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr out = mercury_current_text_output();
    if (ML_fprintf(out, ""%"" MR_INTEGER_LENGTH_MODIFIER ""d"", Val) < 0) {
        mercury_output_error(out);
    }
").

:- pragma foreign_proc("C",
    io.write_float(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
    MercuryFilePtr out;

    MR_sprintf_float(buf, Val);
    out = mercury_current_text_output();
    if (ML_fprintf(out, ""%s"", buf) < 0) {
        mercury_output_error(out);
    }
").

:- pragma foreign_proc("C",
    io.write_byte(Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    /* call putc with a strictly non-negative byte-sized integer */
    if (MR_PUTCH(*mercury_current_binary_output(),
        (int) ((unsigned char) Byte)) < 0)
    {
        mercury_output_error(mercury_current_text_output());
    }
").

io.write_bitmap(Bitmap, !IO) :-
    io.binary_output_stream(Stream, !IO),
    io.write_bitmap(Stream, Bitmap, !IO).

io.write_bitmap(Bitmap, Start, NumBytes, !IO) :-
    io.binary_output_stream(Stream, !IO),
    io.write_bitmap(Stream, Bitmap, Start, NumBytes, !IO).

:- pragma foreign_proc("C",
    io.flush_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr out = mercury_current_text_output();
    if (MR_FLUSH(*out) < 0) {
        mercury_output_error(out);
    }
").

:- pragma foreign_proc("C",
    io.flush_binary_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr out = mercury_current_binary_output();
    if (MR_FLUSH(*out) < 0) {
        mercury_output_error(out);
    }
").

:- pragma foreign_proc("C#",
    io.write_string(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_print_string(io.mercury_current_text_output, Message);
").

:- pragma foreign_proc("C#",
    io.write_char(Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates,
        may_not_duplicate],
"
    /* See mercury_output_string() for comments */
    if (io.mercury_current_text_output.writer == null) {
        io.mercury_current_text_output.writer =
            new System.IO.StreamWriter(io.mercury_current_text_output.stream,
                text_encoding);
    }
    System.IO.TextWriter w = io.mercury_current_text_output.writer;
    if (Character == '\\n') {
        switch (io.mercury_current_text_output.line_ending) {
        case io.ML_line_ending_kind.ML_raw_binary:
        case io.ML_line_ending_kind.ML_Unix_line_ending:
            mercury_write_codepoint(w, Character);
            break;
        case io.ML_line_ending_kind.ML_OS_line_ending:
            w.WriteLine("""");
            break;
        }
        io.mercury_current_text_output.line_number++;
    } else {
        mercury_write_codepoint(w, Character);
    }
").

:- pragma foreign_proc("C#",
    io.write_int(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_print_string(io.mercury_current_text_output, Val.ToString());
").

:- pragma foreign_proc("C#",
    io.write_byte(Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_binary_output.stream.WriteByte(
        System.Convert.ToByte(Byte));
").

:- pragma foreign_proc("C#",
    io.flush_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_text_output.stream.Flush();
").

:- pragma foreign_proc("C#",
    io.flush_binary_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_binary_output.stream.Flush();
").

:- pragma foreign_proc("Java",
    io.write_string(Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_text_output.get().write_or_throw(Message);
").
:- pragma foreign_proc("Java",
    io.write_char(Chr::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.MR_TextOutputFile stream = io.mercury_current_text_output.get();
    char[] buf = java.lang.Character.toChars(Chr);
    for (char c : buf) {
        stream.put_or_throw(c);
    }
").
:- pragma foreign_proc("Java",
    io.write_int(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_text_output.get().write_or_throw(Integer.toString(Val));
").
:- pragma foreign_proc("Java",
    io.write_float(Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_text_output.get().write_or_throw(Double.toString(Val));
").

:- pragma foreign_proc("Java",
    io.write_byte(Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_binary_output.get().put_or_throw((byte) Byte);
").

:- pragma foreign_proc("Java",
    io.flush_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_text_output.get().flush_or_throw();
").

:- pragma foreign_proc("Java",
    io.flush_binary_output(_IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_current_binary_output.get().flush_or_throw();
").

:- pragma foreign_proc("Erlang",
    io.write_string(Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    Stream = ?ML_get_current_text_output,
    mercury__io:mercury_write_string(Stream, Message)
").
:- pragma foreign_proc("Erlang",
    io.write_char(Character::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    Stream = ?ML_get_current_text_output,
    mercury__io:mercury_write_char(Stream, Character)
").
:- pragma foreign_proc("Erlang",
    io.write_int(Val::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    Stream = ?ML_get_current_text_output,
    mercury__io:mercury_write_int(Stream, Val)
").

:- pragma foreign_proc("Erlang",
    io.write_byte(Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    Stream = ?ML_get_current_binary_output,
    mercury__io:mercury_write_char(Stream, Byte)
").

:- pragma foreign_proc("Erlang",
    io.flush_output(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    Stream = ?ML_get_current_text_output,
    mercury__io:mercury_sync(Stream)
").
:- pragma foreign_proc("Erlang",
    io.flush_binary_output(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    Stream = ?ML_get_current_binary_output,
    mercury__io:mercury_sync(Stream)
").

io.write_float(Float, !IO) :-
    io.write_string(string.float_to_string(Float), !IO).

%-----------------------------------------------------------------------------%
%
% Moving about binary streams
%

:- pred whence_to_int(io.whence::in, int::out) is det.

whence_to_int(set, 0).
whence_to_int(cur, 1).
whence_to_int(end, 2).

io.seek_binary_input(binary_input_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    io.seek_binary_2(Stream, Flag, Offset, !IO).

io.seek_binary_output(binary_output_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    io.seek_binary_2(Stream, Flag, Offset, !IO).

:- pred io.seek_binary_2(io.stream::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.seek_binary_2(Stream::in, Flag::in, Off::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };

    /* XXX should check for failure */
    /* XXX should also check if the stream is seekable */
    if (MR_IS_FILE_STREAM(*Stream)) {
        fseek(MR_file(*Stream), Off, seek_flags[Flag]);
    } else {
        mercury_io_error(Stream, ""io.seek_binary_2: unseekable stream"");
    }
").

io.binary_input_stream_offset(binary_input_stream(Stream), Offset, !IO) :-
    io.binary_stream_offset_2(Stream, Offset, !IO).

io.binary_output_stream_offset(binary_output_stream(Stream), Offset, !IO) :-
    io.binary_stream_offset_2(Stream, Offset, !IO).

:- pred io.binary_stream_offset_2(io.stream::in, int::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.binary_stream_offset_2(Stream::in, Offset::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /* XXX should check for failure */
    /* XXX should check if the stream is tellable */
    if (MR_IS_FILE_STREAM(*Stream)) {
        Offset = ftell(MR_file(*Stream));
    } else {
        mercury_io_error(Stream,
            ""io.primitive_binary_stream_offset: untellable stream"");
    }
").

%-----------------------------------------------------------------------------%
%
% Output predicates (with output to the specified stream)
%

io.write_string(output_stream(Stream), Message, !IO) :-
    io.write_string_2(Stream, Message, !IO).

:- pred io.write_string_2(io.stream::in, string::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.write_string_2(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    mercury_print_string(Stream, Message);
").

io.write_char(output_stream(Stream), Character, !IO) :-
    io.write_char_2(Stream, Character, !IO).

:- pred io.write_char_2(io.stream::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.write_char_2(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    if (Character <= 0x7f) {
        if (MR_PUTCH(*Stream, Character) < 0) {
            mercury_output_error(Stream);
        }
        if (Character == '\\n') {
            MR_line_number(*Stream)++;
        }
    } else {
        char    buf[5];
        size_t  len;
        int     i;
        len = MR_utf8_encode(buf, Character);
        for (i = 0; i < len; i++) {
            if (MR_PUTCH(*Stream, buf[i]) < 0) {
                mercury_output_error(Stream);
                break;
            }
        }
    }
").

io.write_int(output_stream(Stream), Val, !IO) :-
    io.write_int_2(Stream, Val, !IO).

:- pred io.write_int_2(io.stream::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.write_int_2(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" MR_INTEGER_LENGTH_MODIFIER ""d"", Val) < 0) {
        mercury_output_error(Stream);
    }
").

io.write_float(output_stream(Stream), Val, !IO) :-
    io.write_float_2(Stream, Val, !IO).

:- pred io.write_float_2(io.stream::in, float::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.write_float_2(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
    MR_sprintf_float(buf, Val);
    if (ML_fprintf(Stream, ""%s"", buf) < 0) {
        mercury_output_error(Stream);
    }
").

io.write_byte(binary_output_stream(Stream), Byte, !IO) :-
    io.write_byte_2(Stream, Byte, !IO).

:- pred io.write_byte_2(io.stream::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.write_byte_2(Stream::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    /* call putc with a strictly non-negative byte-sized integer */
    if (MR_PUTCH(*Stream, (int) ((unsigned char) Byte)) < 0) {
        mercury_output_error(Stream);
    }
").

io.write_bitmap(binary_output_stream(Stream), Bitmap, !IO) :-
    ( NumBytes = Bitmap ^ num_bytes ->
        io.do_write_bitmap(Stream, Bitmap, 0, NumBytes, !IO)
    ;
        error("io.write_bitmap: bitmap contains partial final byte")
    ).

io.write_bitmap(binary_output_stream(Stream), Bitmap, Start, NumBytes, !IO) :-
    (
        byte_in_range(Bitmap, Start),
        byte_in_range(Bitmap, Start + NumBytes - 1)
    ->
        io.do_write_bitmap(Stream, Bitmap, Start, NumBytes, !IO)
    ;
        bitmap.throw_bounds_error(Bitmap, "io.write_bitmap",
            Start * bits_per_byte, NumBytes * bits_per_byte)
    ).

:- pred io.do_write_bitmap(io.stream, bitmap, int, int, io, io).
%:- mode io.do_write_bitmap(in, bitmap_ui, in, in, di, uo) is det.
:- mode io.do_write_bitmap(in, in, in, in, di, uo) is det.
:- pragma promise_pure(io.do_write_bitmap/6).

    % Default implementation for C# and Java.
io.do_write_bitmap(Stream, Bitmap, Start, Length, !IO) :-
    ( Length > 0 ->
        io.write_byte(binary_output_stream(Stream),
            Bitmap ^ unsafe_byte(Start), !IO),
        io.do_write_bitmap(Stream, Bitmap, Start + 1, Length - 1, !IO)
    ;
        true
    ).

:- pragma foreign_proc("C",
    io.do_write_bitmap(Stream::in, Bitmap::in, Start::in, Length::in,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        no_sharing],
"
    MR_Integer bytes_written =
        (MR_Integer)MR_WRITE(*Stream, Bitmap->elements + Start, Length);
    if (bytes_written != Length) {
        mercury_io_error(Stream, \"Error writing bitmap.\");
    }
").

io.flush_output(output_stream(Stream), !IO) :-
    io.flush_output_2(Stream, !IO).

:- pred io.flush_output_2(io.stream::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.flush_output_2(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    if (MR_FLUSH(*Stream) < 0) {
        mercury_output_error(Stream);
    }
").

io.flush_binary_output(binary_output_stream(Stream), !IO) :-
    io.flush_binary_output_2(Stream, !IO).

:- pred io.flush_binary_output_2(io.stream::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.flush_binary_output_2(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    if (MR_FLUSH(*Stream) < 0) {
        mercury_output_error(Stream);
    }
").

:- pragma foreign_proc("C#",
    io.write_string_2(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    io.mercury_print_string(Stream, Message);
").

:- pragma foreign_proc("C#",
    io.write_char_2(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates,
        may_not_duplicate],
"
    io.MR_MercuryFileStruct stream = Stream;
    /* See mercury_output_string() for comments */
    if (stream.writer == null) {
        stream.writer = new System.IO.StreamWriter(stream.stream,
            text_encoding);
    }
    System.IO.TextWriter w = stream.writer;
    if (Character == '\\n') {
        switch (stream.line_ending) {
        case io.ML_line_ending_kind.ML_raw_binary:
        case io.ML_line_ending_kind.ML_Unix_line_ending:
            mercury_write_codepoint(w, Character);
            break;
        case io.ML_line_ending_kind.ML_OS_line_ending:
            w.WriteLine("""");
            break;
        }
        stream.line_number++;
    } else {
        mercury_write_codepoint(w, Character);
    }
").

:- pragma foreign_proc("C#",
    io.write_int_2(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    io.mercury_print_string(Stream, Val.ToString());
}").

:- pragma foreign_proc("C#",
    io.write_byte_2(Stream::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    Stream.stream.WriteByte(System.Convert.ToByte(Byte));
}").

:- pragma foreign_proc("C#",
    io.flush_output_2(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    Stream.stream.Flush();
}").

:- pragma foreign_proc("C#",
    io.flush_binary_output_2(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"{
    Stream.stream.Flush();
}").

:- pragma foreign_proc("Java",
    io.seek_binary_2(Stream::in, Flag::in, Off::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    try {
        ((io.MR_BinaryFile) Stream).seek_binary(Flag, Off);
    } catch (java.io.IOException e) {
        io.ML_throw_io_error(e.getMessage());
    }
").

:- pragma foreign_proc("Java",
    io.binary_stream_offset_2(Stream::in, Offset::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    try {
        Offset = ((io.MR_BinaryFile) Stream).getOffset();
    } catch (java.io.IOException e) {
        Offset = -1;
    }
").

:- pragma foreign_proc("Java",
    io.write_string_2(Stream::in, Message::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    ((io.MR_TextOutputFile) Stream).write_or_throw(Message);
").

:- pragma foreign_proc("Java",
    io.write_char_2(Stream::in, Character::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    char[] buf = java.lang.Character.toChars(Character);
    for (char c : buf) {
        ((io.MR_TextOutputFile) Stream).put_or_throw(c);
    }
").

:- pragma foreign_proc("Java",
    io.write_int_2(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    ((io.MR_TextOutputFile) Stream).write_or_throw(String.valueOf(Val));
").

:- pragma foreign_proc("Java",
    io.write_float_2(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    ((io.MR_TextOutputFile) Stream).write_or_throw(String.valueOf(Val));
").

:- pragma foreign_proc("Java",
    io.write_byte_2(Stream::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    ((io.MR_BinaryOutputFile) Stream).put_or_throw((byte) Byte);
").

:- pragma foreign_proc("Java",
    io.flush_output_2(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    ((io.MR_TextOutputFile) Stream).flush_or_throw();
").

:- pragma foreign_proc("Java",
    io.flush_binary_output_2(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    ((io.MR_BinaryOutputFile) Stream).flush_or_throw();
").

:- pragma foreign_proc("Erlang",
    io.seek_binary_2(Stream::in, Flag::in, Off::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    % Constants from whence_to_int.
    case Flag of
        0 -> Loc = {bof, Off};
        1 -> Loc = {cur, Off};
        2 -> Loc = {eof, Off}
    end,
    mercury__io:mercury_set_pos(Stream, Loc)
    % XXX what to do on error?
").

:- pragma foreign_proc("Erlang",
    io.binary_stream_offset_2(Stream::in, Offset::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    Offset = mercury__io:mercury_get_pos(Stream)
").

:- pragma foreign_proc("Erlang",
    io.write_char_2(Stream::in, Character::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    mercury__io:mercury_write_char(Stream, Character)
").

:- pragma foreign_proc("Erlang",
    io.write_int_2(Stream::in, Val::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    mercury__io:mercury_write_int(Stream, Val)
").

:- pragma foreign_proc("Erlang",
    io.write_string_2(Stream::in, Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    mercury__io:mercury_write_string(Stream, Message)
").

:- pragma foreign_proc("Erlang",
    io.write_byte_2(Stream::in, Byte::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io, terminates],
"
    mercury__io:mercury_write_char(Stream, Byte)
").

:- pragma foreign_proc("Erlang",
    io.flush_output_2(Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    mercury__io:mercury_sync(Stream)
").

:- pragma foreign_proc("Erlang",
    io.flush_binary_output_2(Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        terminates],
"
    mercury__io:mercury_sync(Stream)
").

io.write_float_2(Stream, Float, !IO) :-
    io.write_string_2(Stream, string.float_to_string(Float), !IO).

%----------------------------------------------------------------------------%
%
% Stream predicates
%

:- pragma foreign_export("C", io.stdin_stream_2(out, di, uo),
    "ML_io_stdin_stream").
:- pragma foreign_export("IL", io.stdin_stream_2(out, di, uo),
    "ML_io_stdin_stream").
:- pragma foreign_export("C", io.stdout_stream_2(out, di, uo),
    "ML_io_stdout_stream").
:- pragma foreign_export("IL", io.stdout_stream_2(out, di, uo),
    "ML_io_stdout_stream").
:- pragma foreign_export("C", io.stderr_stream_2(out, di, uo),
    "ML_io_stderr_stream").
:- pragma foreign_export("IL", io.stderr_stream_2(out, di, uo),
    "ML_io_stderr_stream").

io.stdin_stream = input_stream(io.stdin_stream_2).

:- func io.stdin_stream_2 = io.stream.
:- pragma foreign_proc("C",
    io.stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdin;
").

:- pragma foreign_proc("C#",
    io.stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdin;
").

:- pragma foreign_proc("Java",
    io.stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdin;
").

io.stdin_stream(input_stream(Stream), !IO) :-
    io.stdin_stream_2(Stream, !IO).

:- pred io.stdin_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdin;
").

io.stdout_stream = output_stream(io.stdout_stream_2).

:- func io.stdout_stream_2 = io.stream.
:- pragma foreign_proc("C",
    io.stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, does_not_affect_liveness,
        no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdout;
").

:- pragma foreign_proc("C#",
    io.stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdout;
").

:- pragma foreign_proc("Java",
    io.stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdout;
").

io.stdout_stream(output_stream(Stream), !IO) :-
    io.stdout_stream_2(Stream, !IO).

:- pred io.stdout_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdout;
").

io.stderr_stream = output_stream(io.stderr_stream_2).

:- func io.stderr_stream_2 = io.stream.
:- pragma foreign_proc("C",
    io.stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stderr;
").

:- pragma foreign_proc("C#",
    io.stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stderr;
").

:- pragma foreign_proc("Java",
    io.stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stderr;
").

io.stderr_stream(output_stream(Stream), !IO) :-
    io.stderr_stream_2(Stream, !IO).

:- pred io.stderr_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stderr;
").

io.stdin_binary_stream(binary_input_stream(Stream), !IO) :-
    io.stdin_binary_stream_2(Stream, !IO).

:- pred io.stdin_binary_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdin_binary;
").

io.stdout_binary_stream(binary_output_stream(Stream), !IO) :-
    io.stdout_binary_stream_2(Stream, !IO).

:- pred io.stdout_binary_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdout_binary;
").

io.input_stream(input_stream(Stream), !IO) :-
    io.input_stream_2(Stream, !IO).

:- pred io.input_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_text_input();
").

io.output_stream(output_stream(Stream), !IO) :-
    io.output_stream_2(Stream, !IO).

:- pred io.output_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_text_output();
").

io.binary_input_stream(binary_input_stream(Stream), !IO) :-
    io.binary_input_stream_2(Stream, !IO).

:- pred io.binary_input_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_binary_input();
").

io.binary_output_stream(binary_output_stream(Stream), !IO) :-
    io.binary_output_stream_2(Stream, !IO).

:- pred io.binary_output_stream_2(io.stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_binary_output();
").

:- pragma foreign_proc("C",
    io.get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*mercury_current_text_input());
").

io.get_line_number(input_stream(Stream), LineNum, !IO) :-
    io.get_line_number_2(Stream, LineNum, !IO).

:- pred io.get_line_number_2(io.stream::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*Stream);
").

:- pragma foreign_proc("C",
    io.set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*mercury_current_text_input()) = LineNum;
").

io.set_line_number(input_stream(Stream), LineNum, !IO) :-
    io.set_line_number_2(Stream, LineNum,!IO).

:- pred io.set_line_number_2(io.stream::in, int::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    io.set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*Stream) = LineNum;
").

:- pragma foreign_proc("C",
    io.get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*mercury_current_text_output());
").

io.get_output_line_number(output_stream(Stream), LineNum, !IO) :-
    io.get_output_line_number_2(Stream, LineNum, !IO).

:- pred io.get_output_line_number_2(io.stream::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*Stream);
").

:- pragma foreign_proc("C",
    io.set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*mercury_current_text_output()) = LineNum;
").

io.set_output_line_number(output_stream(Stream), LineNum, !IO) :-
    io.set_output_line_number_2(Stream, LineNum, !IO).

:- pred io.set_output_line_number_2(io.stream::in, int::in,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*Stream) = LineNum;
").

io.set_input_stream(input_stream(NewStream), input_stream(OutStream), !IO) :-
    io.set_input_stream_2(NewStream, OutStream, !IO).

:- pred io.set_input_stream_2(io.stream::in, io.stream::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_text_input();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_text_input_index);
").

io.set_output_stream(output_stream(NewStream), output_stream(OutStream),
        !IO) :-
    io.set_output_stream_2(NewStream, OutStream, !IO).

:- pred io.set_output_stream_2(io.stream::in, io.stream::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_text_output();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_text_output_index);
").

io.set_binary_input_stream(binary_input_stream(NewStream),
        binary_input_stream(OutStream), !IO) :-
    io.set_binary_input_stream_2(NewStream, OutStream, !IO).

:- pred io.set_binary_input_stream_2(io.stream::in, io.stream::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_binary_input();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_binary_input_index);
").

io.set_binary_output_stream(binary_output_stream(NewStream),
        binary_output_stream(OutStream), !IO) :-
    io.set_binary_output_stream_2(NewStream, OutStream, !IO).

:- pred io.set_binary_output_stream_2(io.stream::in, io.stream::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    io.set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_binary_output();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_binary_output_index);
").

:- pragma foreign_proc("C#",
    io.stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdin;
").

:- pragma foreign_proc("C#",
    io.stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdout;
").

:- pragma foreign_proc("C#",
    io.stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stderr;
").

:- pragma foreign_proc("C#",
    io.stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdin_binary;
").

:- pragma foreign_proc("C#",
    io.stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdout_binary;
").

:- pragma foreign_proc("C#",
    io.input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_text_input;
").

:- pragma foreign_proc("C#",
    io.output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_text_output;
").

:- pragma foreign_proc("C#",
    io.binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_binary_input;
").

:- pragma foreign_proc("C#",
    io.binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_binary_output;
").

:- pragma foreign_proc("C#",
    io.get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_input.line_number;
").

:- pragma foreign_proc("C#",
    io.get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = Stream.line_number;
").

:- pragma foreign_proc("C#",
    io.set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_input.line_number = LineNum;
").

:- pragma foreign_proc("C#",
    io.set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
    io.get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_output.line_number;
").

:- pragma foreign_proc("C#",
    io.get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").

:- pragma foreign_proc("C#",
    io.set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_output.line_number = LineNum;
").

:- pragma foreign_proc("C#",
    io.set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
    io.set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_input;
    io.mercury_current_text_input = NewStream;
").

:- pragma foreign_proc("C#",
    io.set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_output;
    io.mercury_current_text_output = NewStream;
").

:- pragma foreign_proc("C#",
    io.set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_input;
    io.mercury_current_binary_input = NewStream;
").

:- pragma foreign_proc("C#",
    io.set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_output;
    io.mercury_current_binary_output = NewStream;
").

:- pragma foreign_proc("Java",
    io.stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdin;
").

:- pragma foreign_proc("Java",
    io.stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdout;
").

:- pragma foreign_proc("Java",
    io.stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stderr;
").

:- pragma foreign_proc("Java",
    io.stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    io.ensure_init_mercury_stdin_binary();
    Stream = io.mercury_stdin_binary;
").

:- pragma foreign_proc("Java",
    io.stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    io.ensure_init_mercury_stdout_binary();
    Stream = io.mercury_stdout_binary;
").

:- pragma foreign_proc("Java",
    io.input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_text_input.get();
").

:- pragma foreign_proc("Java",
    io.output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_text_output.get();
").

:- pragma foreign_proc("Java",
    io.binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_binary_input.get();
").

:- pragma foreign_proc("Java",
    io.binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_binary_output.get();
").

:- pragma foreign_proc("Java",
    io.get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_input.get().line_number;
").

:- pragma foreign_proc("Java",
    io.get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = ((io.MR_TextInputFile) Stream).line_number;
").

:- pragma foreign_proc("Java",
    io.set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_input.get().line_number = LineNum;
").

:- pragma foreign_proc("Java",
    io.set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ((io.MR_TextInputFile) Stream).line_number = LineNum;
").

:- pragma foreign_proc("Java",
    io.get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_output.get().line_number;
").

:- pragma foreign_proc("Java",
    io.get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = ((io.MR_TextOutputFile) Stream).line_number;
").

:- pragma foreign_proc("Java",
    io.set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_output.get().line_number = LineNum;
").

:- pragma foreign_proc("Java",
    io.set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ((io.MR_TextOutputFile) Stream).line_number = LineNum;
").

    % io.set_input_stream(NewStream, OldStream, IO0, IO1)
    % Changes the current input stream to the stream specified.
    % Returns the previous stream.

:- pragma foreign_proc("Java",
    io.set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_input.get();
    io.mercury_current_text_input.set((io.MR_TextInputFile) NewStream);
").

:- pragma foreign_proc("Java",
    io.set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_output.get();
    io.mercury_current_text_output.set((io.MR_TextOutputFile) NewStream);
").

:- pragma foreign_proc("Java",
    io.set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_input.get();
    io.mercury_current_binary_input.set((io.MR_BinaryInputFile) NewStream);
").

:- pragma foreign_proc("Java",
    io.set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_output.get();
    io.mercury_current_binary_output.set((io.MR_BinaryOutputFile) NewStream);
").

:- pragma foreign_proc("Erlang",
    io.stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdin_stream')
").

:- pragma foreign_proc("Erlang",
    io.stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdout_stream')
").

:- pragma foreign_proc("Erlang",
    io.stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stderr_stream')
").

:- pragma foreign_proc("Erlang",
    io.stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdin_stream')
").

:- pragma foreign_proc("Erlang",
    io.stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdout_stream')
").

:- pragma foreign_proc("Erlang",
    io.stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stderr_stream')
").

:- pragma foreign_proc("Erlang",
    io.stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdin_binary_stream')
").

:- pragma foreign_proc("Erlang",
    io.stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdout_binary_stream')
").

:- pragma foreign_proc("Erlang",
    io.input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_input
").

:- pragma foreign_proc("Erlang",
    io.output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_output
").

:- pragma foreign_proc("Erlang",
    io.binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_binary_input
").

:- pragma foreign_proc("Erlang",
    io.binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_binary_output
").

:- pragma foreign_proc("Erlang",
    io.get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_input,
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    io.get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    io.set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_input,
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    io.set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    io.get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_output,
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    io.get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    io.set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_output,
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    io.set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    io.set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_text_input,
    mercury__io:mercury_set_current_text_input(NewStream)
").

:- pragma foreign_proc("Erlang",
    io.set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_text_output,
    mercury__io:mercury_set_current_text_output(NewStream)
").

:- pragma foreign_proc("Erlang",
    io.set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_binary_input,
    mercury__io:mercury_set_current_binary_input(NewStream)
").

:- pragma foreign_proc("Erlang",
    io.set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_binary_output,
    mercury__io:mercury_set_current_binary_output(NewStream)
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
    io.stream::out, io::di, io::uo) is det.

:- pred io.do_open_text(string::in, string::in, int::out, int::out,
    io.stream::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.do_open_text(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    Stream = mercury_open(FileName, Mode, MR_ALLOC_ID);
    if (Stream != NULL) {
        ResultCode = 0;
        StreamId = mercury_next_stream_id();
    } else {
        ResultCode = -1;
        StreamId = -1;
    }
").

:- pragma foreign_proc("C",
    io.do_open_binary(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    Stream = mercury_open(FileName, Mode, MR_ALLOC_ID);
    if (Stream != NULL) {
        ResultCode = 0;
        StreamId = mercury_next_stream_id();
    } else {
        ResultCode = -1;
        StreamId = -1;
    }
").

:- pragma foreign_proc("C#",
    io.do_open_text(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    io.MR_MercuryFileStruct mf = io.mercury_open(FileName, Mode,
        io.ML_default_line_ending);
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
    io.MR_MercuryFileStruct mf = io.mercury_open(FileName, Mode,
        io.ML_line_ending_kind.ML_raw_binary);
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
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        if (Mode.charAt(0) == 'r') {
            Stream = new MR_TextInputFile(
                new java.io.FileInputStream(FileName));
        } else if (Mode.charAt(0) == 'w') {
            Stream = new MR_TextOutputFile(
                new java.io.FileOutputStream(FileName));
        } else if (Mode.charAt(0) == 'a') {
            Stream = new MR_TextOutputFile(
                new java.io.FileOutputStream(FileName, true));
        } else {
            io.ML_throw_io_error(""io.do_open_text: "" +
                ""Invalid open mode"" + "" \\"""" + Mode + ""\\"""");
            throw new Error(""unreachable"");
        }
        StreamId = Stream.id;
        ResultCode = 0;
    } catch (java.lang.Exception e) {
        io.MR_io_exception.set(e);
        Stream = null;
        StreamId = -1;
        ResultCode = -1;
    }
").

:- pragma foreign_proc("Java",
    io.do_open_binary(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        switch (Mode.charAt(0)) {
            case 'r':
                Stream = new io.MR_BinaryInputFile(
                    new java.io.FileInputStream(FileName));
                break;
            case 'w':
                Stream = new io.MR_BinaryOutputFile(
                    new java.io.FileOutputStream(FileName));
                break;
            case 'a':
                Stream = new io.MR_BinaryOutputFile(
                    new java.io.FileOutputStream(FileName, true));
                break;
            default:
                io.ML_throw_io_error(""Invalid file opening mode: "" + Mode);
                Stream = null;
                break;
        }
        StreamId = Stream.id;
        ResultCode = 0;
    } catch (java.lang.Exception e) {
        io.MR_io_exception.set(e);
        Stream = null;
        StreamId = -1;
        ResultCode = -1;
    }
").

:- pragma foreign_proc("Erlang",
    io.do_open_text(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    FileNameStr = binary_to_list(FileName),
    ModeStr = binary_to_list(Mode),

    % Text and binary streams are exactly the same so far.
    case mercury__io:mercury_open_stream(FileNameStr, ModeStr) of
        {ok, Stream} ->
            {'ML_stream', StreamId, _Pid} = Stream,
            ResultCode = 0;
        {error, Reason} ->
            put('MR_io_exception', Reason),
            StreamId = -1,
            Stream = null,
            ResultCode = -1
    end
").

:- pragma foreign_proc("Erlang",
    io.do_open_binary(FileName::in, Mode::in, ResultCode::out,
        StreamId::out, Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    FileNameStr = binary_to_list(FileName),
    ModeStr = binary_to_list(Mode),

    % Text and binary streams are exactly the same so far.
    case mercury__io:mercury_open_stream(FileNameStr, ModeStr) of
        {ok, Stream} ->
            {'ML_stream', StreamId, _Pid} = Stream,
            ResultCode = 0;
        {error, Reason} ->
            put('MR_io_exception', Reason),
            StreamId = -1,
            Stream = null,
            ResultCode = -1
    end
").

io.close_input(input_stream(Stream), !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

io.close_output(output_stream(Stream), !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

io.close_binary_input(binary_input_stream(Stream), !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

io.close_binary_output(binary_output_stream(Stream), !IO) :-
    io.maybe_delete_stream_info(Stream, !IO),
    io.close_stream(Stream, !IO).

:- pred io.close_stream(stream::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.close_stream(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        does_not_affect_liveness, no_sharing],
"
    mercury_close(Stream);
").

:- pragma foreign_proc("C#",
    io.close_stream(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    io.mercury_close(Stream);
").

:- pragma foreign_proc("Java",
    io.close_stream(Stream::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    try {
        Stream.close();
    } catch (java.io.IOException e) {
        io.ML_throw_io_error(e.getMessage());
    }
").

:- pragma foreign_proc("Erlang",
    io.close_stream(Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates],
"
    mercury__io:mercury_close_stream(Stream)
").

% Miscellaneous predicates.

:- pragma foreign_proc("C",
    io.progname(DefaultProgname::in, PrognameOut::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate],
"
    if (MR_progname) {
        MR_make_aligned_string(PrognameOut, MR_progname);
    } else {
        PrognameOut = DefaultProgname;
    }
").

:- pragma foreign_proc("C",
    io.command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate,
        no_sharing],
    % no_sharing is okay because the string elements can't reused.
"{
    int i;

    /*
    ** Convert mercury_argv from a vector to a list.
    */
    i = mercury_argc;
    Args = MR_list_empty_msg(MR_ALLOC_ID);
    while (--i >= 0) {
        Args = MR_string_list_cons_msg((MR_Word) mercury_argv[i], Args,
            MR_ALLOC_ID);
    }
}").

:- pragma foreign_proc("C",
    io.get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ExitStatus = mercury_exit_status;
").

:- pragma foreign_proc("C",
    io.set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    mercury_exit_status = ExitStatus;
").

:- pragma foreign_decl("C", "

/*
** A note regarding the declaration of the environ global variable
** that follows:
**
** The man page (on Linux) says that it should be declared by the user
** program.
**
** On MinGW, environ is a macro (defined in stdlib.h) that expands to a
** function call that returns the user environment; no additional
** declaration is required.
**
** On Mac OS X shared libraries do not have direct access to environ.
** The man page for environ(7) says that we should look it up at
** runtime using _NSGetEnviron().
*/

#if defined(MR_HAVE_ENVIRON) && !defined(MR_MAC_OSX)
    #include <unistd.h>

    #if !defined(MR_MINGW)
        extern char **environ;
    #endif
#endif

#if defined(MR_MAC_OSX)
    #include <crt_externs.h>
#endif

#ifdef MR_HAVE_SPAWN_H
    #include <spawn.h>
#endif
").

:- pragma foreign_proc("C",
    io.call_system_code(Command::in, Status::out, Msg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /*
    ** In multithreaded grades, try to use posix_spawn() instead of system().
    ** There were problems with threads and system() on Linux/glibc, probably
    ** because system() uses fork().
    */
#if defined(MR_THREAD_SAFE) && defined(MR_HAVE_POSIX_SPAWN) && \
        defined(MR_HAVE_ENVIRON)

    char    *argv[4];
    pid_t   pid;
    int     err;
    int     st;

    argv[0] = (char *) ""sh"";
    argv[1] = (char *) ""-c"";
    argv[2] = Command;
    argv[3] = NULL;

    /* Protect `environ' from concurrent modifications. */
    MR_OBTAIN_GLOBAL_LOCK(""io.call_system_code/5"");

    /*
    ** See the comment at the head of the body of preceding foreign_decl
    ** for details of why Mac OS X is different here.
    */
    #if defined(MR_MAC_OSX)
        err = posix_spawn(&pid, ""/bin/sh"", NULL, NULL, argv,
            *_NSGetEnviron());
    #else
        err = posix_spawn(&pid, ""/bin/sh"", NULL, NULL, argv, environ);
    #endif

    MR_RELEASE_GLOBAL_LOCK(""io.call_system_code/5"");

    if (err != 0) {
        /* Spawn failed. */
        Status = 127;
        ML_maybe_make_err_msg(MR_TRUE, errno,
            ""error invoking system command: "",
            MR_ALLOC_ID, MR_TRUE, Msg);
    } else {
        /* Wait for the spawned process to exit. */
        do {
            err = waitpid(pid, &st, 0);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == -1) {
            Status = 127;
            ML_maybe_make_err_msg(MR_TRUE, errno,
                ""error invoking system command: "",
                MR_ALLOC_ID, MR_TRUE, Msg);
        } else {
            Status = st;
            Msg = MR_make_string_const("""");
        }
    }

#else   /* !MR_THREAD_SAFE || !MR_HAVE_POSIX_SPAWN || !MR_HAVE_ENVIRON */

  #ifdef MR_WIN32
    Status = _wsystem(ML_utf8_to_wide(Command));
  #else
    Status = system(Command);
  #endif

    if (Status == -1) {
        /*
        ** Return values of 127 or -1 from system() indicate that
        ** the system call failed. Don't return -1, as -1 indicates
        ** that the system call was killed by signal number 1.
        */
        Status = 127;
        ML_maybe_make_err_msg(MR_TRUE, errno,
            ""error invoking system command: "",
            MR_ALLOC_ID, MR_TRUE, Msg);
    } else {
        Msg = MR_make_string_const("""");
    }

#endif  /* !MR_THREAD_SAFE || !MR_HAVE_POSIX_SPAWN || !MR_HAVE_ENVIRON */
").

:- pragma foreign_proc("Erlang",
    io.call_system_code(Command::in, Status::out, Msg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    % XXX this is bad
    % 1. the command cannot receive input
    % 2. output doesn't come out until process finishes
    % 3. the error code is returned in an inefficient way
    % 4. standard output and standard error are always tied together
    %
    CommandStr = binary_to_list(Command),
    OutputCode = os:cmd(CommandStr ++ ""; echo -n $?""),
    case string:rchr(OutputCode, $\\n) of
        0 ->
            Code = OutputCode;
        NL ->
            {Output, [$\\n, Code]} = lists:split(NL - 1, OutputCode),
            io:put_chars(Output)
    end,
    {Status, []} = string:to_integer(Code),
    case Status =:= 0 of
        true ->
            Msg = <<>>;
        false ->
            Msg = <<""error invoking system command"">>
    end
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
        % The process was killed by a signal.
        Status = -(Status0 /\ 0xff)
    ;
        % The process terminated normally.
        Status = (Status0 /\ 0xff00) >> 8
    ).

:- pragma foreign_proc("C",
    io.handle_system_command_exit_code(Status0::in) = (Status::out),
    [will_not_call_mercury, thread_safe, promise_pure, does_not_affect_liveness,
        no_sharing],
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
    Args = list.empty_list();
    // We don't get the 0th argument: it is the executable name.
    while (--i > 0) {
        Args = list.cons(arg_vector[i], Args);
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
        string command, arguments;
        if (index > 0) {
            command = Command.Substring(0, index);
            arguments = Command.Remove(0, index + 1);
        } else {
            command = Command;
            arguments = """";
        }

        // debugging...
        // System.Console.Out.WriteLine(
        //  ""[command = "" + command + ""]"");
        // System.Console.Out.WriteLine(
        //  ""[arguments = "" + arguments + ""]"");

        System.Diagnostics.Process process = new System.Diagnostics.Process();
        // Never interpret the command as a document to open with whatever
        // application is registered for that document type.  This also
        // prevents creating a new window for console programs on Windows.
        process.StartInfo.UseShellExecute = false;
        process.StartInfo.FileName = command;
        process.StartInfo.Arguments = arguments;
        process.Start();
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
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    PrognameOut = jmercury.runtime.JavaInternal.progname;
").

:- pragma foreign_proc("Java",
    io.get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    ExitStatus = jmercury.runtime.JavaInternal.exit_status;
").

:- pragma foreign_proc("Java",
    io.set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    jmercury.runtime.JavaInternal.exit_status = ExitStatus;
").

:- pragma foreign_proc("Erlang",
    io.command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    ArgStrings = init:get_plain_arguments(),
    Args = lists:map(fun list_to_binary/1, ArgStrings)
").

:- pragma foreign_proc("Erlang",
    io.get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    'ML_erlang_global_server' ! {get_exit_status, self()},
    receive
        {get_exit_status_ack, ExitStatus} ->
            void
    end
").

:- pragma foreign_proc("Erlang",
    io.set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    'ML_erlang_global_server' ! {set_exit_status, ExitStatus}
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
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    String[] arg_vector = jmercury.runtime.JavaInternal.args;

    if (ArgNum < arg_vector.length && ArgNum >= 0) {
        Arg = arg_vector[ArgNum];
        SUCCESS_INDICATOR = true;
    } else {
        Arg = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    io.call_system_code(Command::in, Status::out, Msg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    boolean has_sh;
    try {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkExec(""/bin/sh"");
            has_sh = true;
        } else {
            // If there is no security manager installed we just check if the
            // file exists.
            has_sh = new java.io.File(""/bin/sh"").exists();
        }
    } catch (java.lang.Exception e) {
        has_sh = false;
    }

    try {
        // Emulate system() if /bin/sh is available.
        java.lang.Process process;
        if (has_sh) {
            final String[] args = {""/bin/sh"", ""-c"", Command};
            process = java.lang.Runtime.getRuntime().exec(args);
        } else {
            process = java.lang.Runtime.getRuntime().exec(Command);
        }

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
        Msg = """";

        // The stdin StreamPipe is killed off after the Process is finished
        // so as not to waste CPU cycles with a pointless thread.
        stdin.interrupt();

        // Wait for all the outputs to be written.
        stdout.join();
        stderr.join();

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

%-----------------------------------------------------------------------------%

% io.getenv and io.setenv.

:- pragma foreign_decl("C", "
#include <stdlib.h> /* for getenv() and putenv() */
").

:- pragma foreign_proc("C",
    io.getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_WIN32
    wchar_t *ValueW = _wgetenv(ML_utf8_to_wide(Var));
    if (ValueW != NULL) {
        Value = ML_wide_to_utf8(ValueW, MR_ALLOC_ID);
    } else {
        Value = NULL;
    }
#else
    Value = getenv(Var);
#endif
    SUCCESS_INDICATOR = (Value != 0);
").

:- pragma foreign_proc("C#",
    io.getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io],
"
    Value = System.Environment.GetEnvironmentVariable(Var);
    SUCCESS_INDICATOR = (Value != null);
").

:- pragma foreign_proc("Java",
    io.getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    Value = System.getenv(Var);
    SUCCESS_INDICATOR = (Value != null);
").

:- pragma foreign_proc("Erlang",
    io.getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io],
"
    case os:getenv(binary_to_list(Var)) of
        false ->
            SUCCESS_INDICATOR = false,
            Value = <<>>;
        ValueStr ->
            SUCCESS_INDICATOR = true,
            Value = list_to_binary(ValueStr)
    end
").

io.setenv(Var, Value) :-
    impure io.putenv(Var ++ "=" ++ Value).

    % io.putenv(VarString): If VarString is a string of the form "name=value",
    % sets the environment variable name to the specified value. Fails if
    % the operation does not work. This should only be called from io.setenv.
    %
:- impure pred io.putenv(string::in) is semidet.

:- pragma foreign_proc("C",
    io.putenv(VarAndValue::in),
    [will_not_call_mercury, tabled_for_io, does_not_affect_liveness,
        no_sharing],
"
#ifdef MR_WIN32
    SUCCESS_INDICATOR = (_wputenv(ML_utf8_to_wide(VarAndValue)) == 0);
#else
    SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);
#endif
").

:- pragma foreign_proc("C#",
    io.setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io],
"
    try {
        System.Environment.SetEnvironmentVariable(Var, Value);
        SUCCESS_INDICATOR = true;
    } catch (System.Exception) {
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("C#",
    io.putenv(_VarAndValue::in),
    [will_not_call_mercury, tabled_for_io],
"
    // This procedure should never be called, as io.setenv/2 has been
    // implemented directly for C#.
    // This implementation is included only to suppress warnings.

    throw new System.Exception(""io.putenv/1 not implemented for C#"");
").

:- pragma foreign_proc("Java",
    io.setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    // Java does not provide a method to set environment variables, only a way
    // to modify the environment when creating a new process.

    // Avoid warning: Var, Value
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Java",
    io.putenv(VarAndValue::in),
    [will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    // This procedure should never be called, as io.setenv/2 has been
    // implemented directly for Java.
    // This implementation is included only to suppress warnings.

    io.ML_throw_io_error(
        ""io.putenv/1 not implemented for Java: "" + VarAndValue);
").

:- pragma foreign_proc("Erlang",
    io.setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io],
"
    VarStr = binary_to_list(Var),
    ValueStr = binary_to_list(Value),
    os:putenv(VarStr, ValueStr),
    SUCCESS_INDICATOR = true
").

:- pragma foreign_proc("Erlang",
    io.putenv(VarAndValue::in),
    [will_not_call_mercury, tabled_for_io],
"
    % This procedure should never be called, as io.setenv/2 has been
    % implemented directly for Erlang.
    % This implementation is included only to suppress warnings.

    throw({""io.putenv/1 not implemented for Erlang: "" ++ VarAndValue})
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

%-----------------------------------------------------------------------------%

:- pred io.do_make_temp(string::in, string::in, string::in,
    string::out, int::out, string::out, io::di, io::uo) is det.

% XXX The code for io.make_temp assumes POSIX. It uses the functions open(),
% close(), and getpid() and the macros EEXIST, O_WRONLY, O_CREAT, and O_EXCL.
% We should be using conditional compilation here to avoid these POSIX
% dependencies.

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
        Error::out, ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
#ifdef MR_HAVE_MKSTEMP
    int err, fd;

    FileName = MR_make_string(MR_ALLOC_ID, ""%s%s%.5sXXXXXX"",
        Dir, Sep, Prefix);
    fd = mkstemp(FileName);
    if (fd == -1) {
        ML_maybe_make_err_msg(MR_TRUE, errno,
            ""error opening temporary file: "", MR_ALLOC_ID,
            MR_TRUE, ErrorMessage);
        Error = -1;
    } else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        ML_maybe_make_err_msg(err, errno,
            ""error closing temporary file: "", MR_ALLOC_ID,
            MR_TRUE, ErrorMessage);
        Error = err;
    }
#else
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
    int     flags;

    len = strlen(Dir) + 1 + 5 + 3 + 1 + 3 + 1;
    /* Dir + / + Prefix + counter_high + . + counter_low + \\0 */
    MR_offset_incr_hp_atomic_msg(filename_word, 0,
        (len + sizeof(MR_Word)) / sizeof(MR_Word),
        MR_ALLOC_ID, ""string.string/0"");
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
        flags = O_WRONLY | O_CREAT | O_EXCL;
        do {
            #ifdef MR_WIN32
                fd = _wopen(ML_utf8_to_wide(FileName), flags, 0600);
            #else
                fd = open(FileName, flags, 0600);
            #endif
        } while (fd == -1 && MR_is_eintr(errno));
        num_tries++;
        ML_io_tempnam_counter += (1 << num_tries);
    } while (fd == -1 && errno == EEXIST &&
        num_tries < ML_MAX_TEMPNAME_TRIES);
    if (fd == -1) {
        ML_maybe_make_err_msg(MR_TRUE, errno,
            ""error opening temporary file: "", MR_ALLOC_ID,
            MR_TRUE, ErrorMessage);
        Error = -1;
    }  else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        ML_maybe_make_err_msg(err, errno,
            ""error closing temporary file: "", MR_ALLOC_ID,
            MR_TRUE, ErrorMessage);
        Error = err;
    }
#endif
").

:- pragma foreign_proc("C#",
    io.do_make_temp(_Dir::in, _Prefix::in, _Sep::in, FileName::out,
        Error::out, ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
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
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        java.io.File tmpdir = new java.io.File(
            java.lang.System.getProperty(""java.io.tmpdir""));
        FileName = java.io.File.createTempFile(""mtmp"", null, tmpdir).
            getName();
    } catch (java.lang.Exception e) {
        io.ML_throw_io_error(e.getMessage());
        FileName = null;
    }
").

:- pragma foreign_proc("Java",
    io.make_temp(Dir::in, Prefix::in, FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
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
        io.ML_throw_io_error(e.getMessage());
        FileName = null;
    }
").

:- pragma foreign_proc("Erlang",
    io.do_make_temp(Dir::in, Prefix::in, Sep::in, FileName::out,
        Error::out, ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
    DirStr = binary_to_list(Dir),
    PrefixStr = binary_to_list(Prefix),
    SepStr = binary_to_list(Sep),

    % Constructs a temporary name by concatenating Dir, Sep, Prefix
    % three hex digits, '.', and 3 more hex digits.

    % XXX we should try to mix in the Erlang process id in case two Erlang
    % processes from the same Unix process are trying to create temporary files
    % at the same time (it's not as far-fetched as it sounds, e.g. mmc --make)

    MaxTries = 24,

    {A1, A2, A3} = now(),
    case string:to_integer(os:getpid()) of
        {Pid, []} ->
            void;
        _ ->
            Pid = 0
    end,
    Seed = {A1 + Pid, A2, A3},

    case
        mercury__io:'ML_do_make_temp_2'(DirStr, PrefixStr, SepStr,
            MaxTries, Seed)
    of
        {ok, FileName0} ->
            FileName = list_to_binary(FileName0),
            Error = 0,
            ErrorMessage = <<>>;
        {error, Reason} ->
            FileName = <<>>,
            Error = -1,
            ErrorMessage = list_to_binary(Reason)
    end
").

:- pragma foreign_decl("Erlang", local, "
    -export(['ML_do_make_temp_2'/5]).
").
:- pragma foreign_code("Erlang", "
    'ML_do_make_temp_2'(_, _, _, 0, _) ->
        {error, ""error opening temporary file""};
    'ML_do_make_temp_2'(Dir, Prefix, Sep, Tries, Seed0) ->
        {R1, Seed1} = random:uniform_s(16#1000, Seed0),
        {R2, Seed}  = random:uniform_s(16#1000, Seed1),
        FileName = lists:flatten(io_lib:format(""~s~s~s~3.16.0B.~3.16.0B"",
            [Dir, Sep, Prefix, R1, R2])),
        case filelib:is_file(FileName) of
            true ->
                'ML_do_make_temp_2'(Dir, Prefix, Sep, Tries - 1, Seed);
            false ->
                case file:open(FileName, [write, {encoding, utf8}]) of
                    {ok, IoDevice} ->
                        case file:close(IoDevice) of
                            ok ->
                                {ok, FileName};
                            {error, Reason} ->
                                {error, file:format_error(Reason)}
                        end;
                    {error, _} ->
                        'ML_do_make_temp_2'(Dir, Prefix, Sep, Tries - 1, Seed)
                end
        end.
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#include <string.h>
#include <errno.h>

/*
** ML_maybe_make_err_msg(was_error, errno, msg, alloc_id, req_lock, error_msg):
** if `was_error' is true, then append `msg' and `strerror(errno)'
** to give `error_msg'; otherwise, set `error_msg' to "".
** `req_lock' must be true iff the caller is marked `thread_safe' as the
** underlying strerror() function is not thread-safe.
**
** WARNING: this must only be called when the `hp' register is valid.
** That means it must only be called from procedures declared
** `[will_not_call_mercury, promise_pure]'.
**
** This is defined as a macro rather than a C function
** to avoid worrying about the `hp' register being
** invalidated by the function call.
*/

#define ML_maybe_make_err_msg(was_error, error, msg, alloc_id, req_lock,   \\
            error_msg)                                                     \\
    do {                                                                   \\
        char    *errno_msg;                                                \\
        size_t  total_len;                                                 \\
        MR_Word tmp;                                                       \\
                                                                           \\
        if (was_error) {                                                   \\
            if (req_lock) {                                                \\
                MR_OBTAIN_GLOBAL_LOCK(""ML_maybe_make_err_msg"");          \\
            }                                                              \\
            errno_msg = strerror(error);                                   \\
            total_len = strlen(msg) + strlen(errno_msg);                   \\
            MR_offset_incr_hp_atomic_msg(tmp, 0,                           \\
                (total_len + sizeof(MR_Word)) / sizeof(MR_Word),           \\
                (alloc_id), ""string.string/0"");                          \\
            (error_msg) = (char *) tmp;                                    \\
            strcpy((error_msg), msg);                                      \\
            strcat((error_msg), errno_msg);                                \\
            if (req_lock) {                                                \\
                MR_RELEASE_GLOBAL_LOCK(""ML_maybe_make_err_msg"");         \\
            }                                                              \\
        } else {                                                           \\
            /*                                                             \\
            ** We can't just return NULL here, because otherwise mdb       \\
            ** will break when it tries to print the string.               \\
            */                                                             \\
            (error_msg) = MR_make_string_const("""");                      \\
        }                                                                  \\
    } while(0)

/*
** ML_maybe_make_win32_err_msg(was_error, error, msg, alloc_id, error_msg):
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
*/
#ifdef MR_WIN32

#define ML_maybe_make_win32_err_msg(was_error, error, msg, alloc_id,        \\
        error_msg)                                                          \\
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
                err_buf = (LPVOID) ""could not retrieve error message"";    \\
            }                                                               \\
            total_len = strlen(msg) + strlen((char *)err_buf);              \\
            MR_incr_hp_atomic_msg(tmp,                                      \\
                (total_len + sizeof(MR_Word)) / sizeof(MR_Word),            \\
                (alloc_id), ""string.string/0"");                           \\
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

#define ML_maybe_make_win32_err_msg(was_error, error, msg, alloc_id, error_msg) \\
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
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_WIN32
    RetVal = _wremove(ML_utf8_to_wide(FileName));
#else
    RetVal = remove(FileName);
#endif
    ML_maybe_make_err_msg(RetVal != 0, errno, ""remove failed: "",
        MR_ALLOC_ID, MR_TRUE, RetStr);
").

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
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
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

:- pragma foreign_proc("Erlang",
    io.remove_file_2(FileName::in, RetVal::out, RetStr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    FileNameStr = binary_to_list(FileName),
    case file:delete(FileNameStr) of
        ok ->
            RetVal = 0,
            RetStr = <<>>;
        {error, Reason} ->
            RetVal = -1,
            ReasonStr = file:format_error(Reason),
            RetStr = list_to_binary([""remove failed: "", ReasonStr])
    end
").

remove_file_recursively(FileName, Res, !IO) :-
    FollowSymLinks = no,
    io.file_type(FollowSymLinks, FileName, ResFileType, !IO),
    (
        ResFileType = ok(FileType),
        (
            FileType = directory,
            dir.foldl2(remove_directory_entry, FileName, ok, Res0, !IO),
            (
                Res0 = ok(MaybeError),
                (
                    MaybeError = ok,
                    io.remove_file(FileName, Res, !IO)
                ;
                    MaybeError = error(Error),
                    Res = error(Error)
                )
            ;
                Res0 = error(_, Error),
                Res = error(Error)
            )
        ;
            ( FileType = regular_file
            ; FileType = symbolic_link
            ; FileType = named_pipe
            ; FileType = socket
            ; FileType = character_device
            ; FileType = block_device
            ; FileType = message_queue
            ; FileType = semaphore
            ; FileType = shared_memory
            ; FileType = unknown
            ),
            io.remove_file(FileName, Res, !IO)
        )
    ;
        ResFileType = error(Error),
        Res = error(Error)
    ).

:- pred remove_directory_entry(string::in, string::in, file_type::in,
    bool::out, io.res::in, io.res::out, io::di, io::uo) is det.

remove_directory_entry(DirName, FileName, _FileType, Continue, _, Res, !IO) :-
    remove_file_recursively(DirName / FileName, Res0, !IO),
    (
        Res0 = ok,
        Res = ok,
        Continue = yes
    ;
        Res0 = error(_),
        Res = Res0,
        Continue = no
    ).

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
        RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_WIN32
    RetVal = _wrename(ML_utf8_to_wide(OldFileName),
        ML_utf8_to_wide(NewFileName));
#else
    RetVal = rename(OldFileName, NewFileName);
#endif
    ML_maybe_make_err_msg(RetVal != 0, errno, ""rename failed: "",
        MR_ALLOC_ID, MR_TRUE, RetStr);
").

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
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
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

:- pragma foreign_proc("Erlang",
    io.rename_file_2(OldFileName::in, NewFileName::in, RetVal::out,
        RetStr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    OldFileNameStr = binary_to_list(OldFileName),
    NewFileNameStr = binary_to_list(NewFileName),
    case file:rename(OldFileNameStr, NewFileNameStr) of
        ok ->
            RetVal = 0,
            RetStr = <<>>;
        {error, Reason} ->
            RetVal = -1,
            ReasonStr = file:format_error(Reason),
            RetStr = list_to_binary([""rename_file failed: "", ReasonStr])
    end
").

io.have_symlinks :- semidet_fail.

:- pragma foreign_proc("C",
    io.have_symlinks,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_HAVE_SYMLINK) && defined(MR_HAVE_READLINK)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pragma foreign_proc("Erlang",
    io.have_symlinks,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    % XXX how do we check without actually trying to make one?
    SUCCESS_INDICATOR = true
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
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_SYMLINK
    Status = (symlink(FileName, LinkFileName) == 0);
#else
    Status = 0;
#endif
").

:- pragma foreign_proc("Erlang",
    io.make_symlink_2(FileName::in, LinkFileName::in, Status::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    FileNameStr = binary_to_list(FileName),
    LinkFileNameStr = binary_to_list(LinkFileName),
    case file:make_symlink(FileNameStr, LinkFileNameStr) of
        ok ->
            Status = 1;
        {error, Reason} ->
            put('MR_io_exception', Reason),
            Status = 0
    end
").

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
        Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
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
            MR_make_aligned_string_copy_msg(TargetFileName, buffer2,
                MR_ALLOC_ID);
            Status = 1;
        }
        MR_free(buffer2);
    } else if (num_chars == -1) {
        TargetFileName = MR_make_string_const("""");
        Error = errno;
        Status = 0;
    } else {
        buffer[num_chars] = '\\0';
        MR_make_aligned_string_copy_msg(TargetFileName, buffer, MR_ALLOC_ID);
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
").

:- pragma foreign_proc("Erlang",
    io.read_symlink_2(FileName::in, TargetFileName::out,
        Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    case file:read_link(binary_to_list(FileName)) of
        {ok, TargetFileNameStr} ->
            TargetFileName = list_to_binary(TargetFileNameStr),
            Status = 1,
            Error = <<>>;
        {error, Reason} ->
            Status = 0,
            TargetFileName = <<>>,
            Error = list_to_binary(file:format_error(Reason))
    end
").

% Since io.have_symlinks will fail for Java, these procedures should never be
% called:

:- pragma foreign_proc("Java",
    io.make_symlink_2(_FileName::in, _LinkFileName::in, _Status::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    if (true) {
        throw new java.lang.RuntimeException(
            ""io.make_symlink_2 not implemented"");
    }
").

:- pragma foreign_proc("Java",
    io.read_symlink_2(_FileName::in, _TargetFileName::out, _Status::out,
        _Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    if (true) {
        throw new java.lang.RuntimeException(
            ""io.read_symlink_2 not implemented"");
    }
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Instances of the stream typeclass
%

:- instance stream.error(io.error) where [
    func(stream.error_message/1) is io.error_message
].

%-----------------------------------------------------------------------------%
%
% Text input streams
%

:- instance stream.stream(io.input_stream, io) where [
    pred(name/4) is io.input_stream_name
].

:- instance stream.input(io.input_stream, io) where [].

:- instance stream.reader(io.input_stream, char, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        io.read_char(Stream, Result0, !IO),
        Result = io.result_to_stream_result(Result0)
    )
].

:- instance stream.reader(io.input_stream, line, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        io.read_line_as_string(Stream, Result0, !IO),
        (
            Result0 = ok(String),
            Result = ok(line(String))
        ;
            Result0 = eof,
            Result = eof
        ;
            Result0 = error(Error),
            Result = error(Error)
        )
    )
].

:- instance stream.reader(io.input_stream, text_file, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        io.read_file_as_string(Stream, Result0, !IO),
        (
            Result0 = ok(String),
            Result = ok(text_file(String))
        ;
            Result0 = error(_PartialString, Error),
            Result = error(Error)
        )
    )
].

:- instance stream.putback(io.input_stream, char, io, io.error) where
[
    pred(unget/4) is io.putback_char
].

:- func io.result_to_stream_result(io.result(T)) = stream.result(T, io.error).

io.result_to_stream_result(ok(T)) = ok(T).
io.result_to_stream_result(eof) = eof.
io.result_to_stream_result(error(Error)) = error(Error).

:- instance stream.line_oriented(io.input_stream, io) where
[
    pred(get_line/4) is io.get_line_number,
    pred(set_line/4) is io.set_line_number
].

%-----------------------------------------------------------------------------%
%
% Text output streams
%

:- instance stream.stream(io.output_stream, io) where [
    pred(name/4) is io.output_stream_name
].

:- instance stream.output(io.output_stream, io) where [
    pred(flush/3) is io.flush_output
].

:- instance stream.writer(io.output_stream, char, io)
    where
[
    pred(put/4) is io.write_char
].

:- instance stream.writer(io.output_stream, float, io)
    where
[
    pred(put/4) is io.write_float
].

:- instance stream.writer(io.output_stream, int, io)
    where
[
    pred(put/4) is io.write_int
].

:- instance stream.writer(io.output_stream, string, io)
    where
[
    pred(put/4) is io.write_string
].

:- instance stream.writer(io.output_stream, univ, io)
    where
[
    pred(put/4) is stream.string_writer.write_univ
].

:- instance stream.line_oriented(io.output_stream, io) where
[
    pred(get_line/4) is io.get_output_line_number,
    pred(set_line/4) is io.set_output_line_number
].

%-----------------------------------------------------------------------------%
%
% Binary input streams
%

:- instance stream.stream(io.binary_input_stream, io)
    where
[
    pred(name/4) is io.binary_input_stream_name
].

:- instance stream.input(io.binary_input_stream, io)
    where [].

:- instance stream.reader(io.binary_input_stream, int, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        io.read_byte(Stream, Result0, !IO),
        Result = io.result_to_stream_result(Result0)
    )
].

:- instance stream.bulk_reader(io.binary_input_stream, int,
        bitmap, io, io.error)
    where
[
    ( bulk_get(Stream, Index, Int, !Store, NumRead, Result, !State) :-
        io.read_bitmap(Stream, Index, Int, !Store, NumRead,
            Result0, !State),
        Result = io.res_to_stream_res(Result0)
    )
].

:- instance stream.putback(io.binary_input_stream, int, io, io.error)
    where
[
    pred(unget/4) is io.putback_byte
].

:- instance stream.seekable(io.binary_input_stream, io)
    where
[
    ( seek(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        io.seek_binary_input(Stream, Whence, OffSet, !IO)
    )
].

:- func stream_whence_to_io_whence(stream.whence) = io.whence.

stream_whence_to_io_whence(set) = set.
stream_whence_to_io_whence(cur) = cur.
stream_whence_to_io_whence(end) = end.

:- func io.res_to_stream_res(io.res) = stream.res(io.error).

io.res_to_stream_res(ok) = ok.
io.res_to_stream_res(error(E)) = error(E).

%-----------------------------------------------------------------------------%
%
% Binary output streams
%

:- instance stream.stream(io.binary_output_stream, io)
    where
[
    pred(name/4) is io.binary_output_stream_name
].

:- instance stream.output(io.binary_output_stream, io)
    where
[
    pred(flush/3) is io.flush_binary_output
].

:- instance stream.writer(io.binary_output_stream, byte, io)
    where
[
    pred(put/4) is io.write_byte
].

:- instance stream.writer(io.binary_output_stream, bitmap, io)
    where
[
    pred(put/4) is io.write_bitmap
].

:- instance stream.writer(io.binary_output_stream, bitmap.slice, io)
    where
[
    ( put(Stream, Slice, !IO) :-
        io.write_bitmap(Stream, Slice ^ slice_bitmap,
            Slice ^ slice_start_byte_index, Slice ^ slice_num_bytes, !IO)
    )
].

:- instance stream.seekable(io.binary_output_stream, io)
    where
[
    ( seek(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        io.seek_binary_output(Stream, Whence, OffSet, !IO)
    )
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
