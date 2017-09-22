%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: io.m.
% Main author: fjh.
% Stability: medium to high.
%
% This file encapsulates all the file I/O.
%
% We implement a purely logical I/O system using non-logical I/O primitives of
% the underlying system. We ensure referential transparency by passing around
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.
:- interface.

:- import_module array.
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

%---------------------------------------------------------------------------%
%
% Exported types.
%

    % The state of the universe.
    %
:- type io.state.

    % An alternative, more concise name for `io.state'.
    %
:- type io == io.state.

    % Opaque handles for text I/O streams.
    %
:- type input_stream.
:- type output_stream.

    % Alternative names for the above.
    %
:- type text_input_stream == input_stream.
:- type text_output_stream == output_stream.

    % Opaque handles for binary I/O streams.
    %
:- type binary_input_stream.
:- type binary_output_stream.

    % A unique identifier for an I/O stream.
    %
:- type stream_id.

    % Various types used for the result from the access predicates.
    %
:- type res
    --->    ok
    ;       error(io.error).

:- type res(T)
    --->    ok(T)
    ;       error(io.error).

    % maybe_partial_res is used where it is possible to return a partial result
    % when an error occurs.
    %
:- type maybe_partial_res(T)
    --->    ok(T)
    ;       error(T, io.error).

:- inst maybe_partial_res(T)
    --->    ok(T)
    ;       error(T, ground).

:- type result
    --->    ok
    ;       eof
    ;       error(io.error).

:- type result(T)
    --->    ok(T)
    ;       eof
    ;       error(io.error).

:- type read_result(T)
    --->    ok(T)
    ;       eof
    ;       error(string, int). % error message, line number

:- type io.error.   % Use error_message to decode it.

    % whence denotes the base for a seek operation.
    %   set - seek relative to the start of the file
    %   cur - seek relative to the current position in the file
    %   end - seek relative to the end of the file.
    %
:- type whence
    --->    set
    ;       cur
    ;       end.

%---------------------------------------------------------------------------%
%
% Text input predicates.
%

    % Read a character (code point) from the current input stream
    % or from the specified stream.
    %
:- pred read_char(io.result(char)::out, io::di, io::uo) is det.
:- pred read_char(io.text_input_stream::in, io.result(char)::out,
    io::di, io::uo) is det.

    % Read a whitespace delimited word from the current input stream
    % or from the specified stream.
    %
:- pred read_word(io.result(list(char))::out, io::di, io::uo) is det.
:- pred read_word(io.text_input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Read a line from the current input stream or from the specified
    % stream, returning the result as a list of characters (code points).
    %
    % See the documentation for `string.line' for the definition of a line.
    %
:- pred read_line(io.result(list(char))::out, io::di, io::uo) is det.
:- pred read_line(io.text_input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Read a line from the current input stream or from the specified
    % stream, returning the result as a string.
    %
    % See the documentation for `string.line' for the definition of a line.
    %
:- pred read_line_as_string(io.result(string)::out, io::di, io::uo) is det.
:- pred read_line_as_string(io.text_input_stream::in, io.result(string)::out,
    io::di, io::uo) is det.

    % Read all the characters (code points) from the current input stream
    % or from the specified stream, until eof or error.
    %
:- pred read_file(io.maybe_partial_res(list(char))::out,
    io::di, io::uo) is det.
:- pred read_file(io.text_input_stream::in,
    io.maybe_partial_res(list(char))::out, io::di, io::uo) is det.

    % Read all the characters (code points) from the current input stream
    % or from the specified stream, until eof or error. Returns the result
    % as a string rather than as a list of char.
    %
    % Returns an error if the file contains a null character, because
    % null characters are not allowed in Mercury strings.
    %
:- pred read_file_as_string(io.maybe_partial_res(string)::out,
    io::di, io::uo) is det.
:- pred read_file_as_string(io.text_input_stream::in,
    io.maybe_partial_res(string)::out, io::di, io::uo) is det.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred input_stream_foldl(pred(char, T, T), T, io.maybe_partial_res(T),
    io, io).
:- mode input_stream_foldl((pred(in, in, out) is det), in, out,
    di, uo) is det.
:- mode input_stream_foldl((pred(in, in, out) is cc_multi), in, out,
    di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred input_stream_foldl_io(pred(char, io, io), io.res, io, io).
:- mode input_stream_foldl_io((pred(in, di, uo) is det), out, di, uo)
    is det.
:- mode input_stream_foldl_io((pred(in, di, uo) is cc_multi), out, di, uo)
    is cc_multi.

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred input_stream_foldl2_io(pred(char, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode input_stream_foldl2_io((pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl2_io((pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error, or the closure returns `no' as
    % its second argument.
    %
:- pred input_stream_foldl2_io_maybe_stop(
    pred(char, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Un-read a character (code point) from the current input stream
    % or from the specified stream.
    % You can put back as many characters as you like.
    % You can even put back something that you didn't actually read.
    %
    % On some systems and backends, only one byte of pushback is guaranteed.
    % `putback_char' will throw an io.error exception if the pushback buffer
    % is full.
    %
:- pred putback_char(char::in, io::di, io::uo) is det.
:- pred putback_char(io.text_input_stream::in, char::in, io::di, io::uo)
    is det.

    % Reads a character (code point) from the specified stream.
    % This interface avoids memory allocation when there is no error.
    %
:- pred read_char_unboxed(io.text_input_stream::in, io.result::out, char::out,
    io::di, io::uo) is det.

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error.
    %
:- pred input_stream_foldl(io.text_input_stream, pred(char, T, T),
    T, io.maybe_partial_res(T), io, io).
:- mode input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error.
    %
:- pred input_stream_foldl_io(io.text_input_stream, pred(char, io, io),
    io.res, io, io).
:- mode input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error.
    %
:- pred input_stream_foldl2_io(io.text_input_stream,
    pred(char, T, T, io, io),
    T, maybe_partial_res(T), io, io).
:- mode input_stream_foldl2_io(in,
    in(pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl2_io(in,
    in(pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error, or the closure returns `no' as
    % its second argument.
    %
:- pred input_stream_foldl2_io_maybe_stop(io.text_input_stream,
    pred(char, bool, T, T, io, io),
    T, maybe_partial_res(T), io, io).
:- mode input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Read a ground term of any type, written using standard Mercury syntax,
    % from the current stream or from the specified input stream.
    % The type of the term read is determined by the context from which
    % `io.read' is called.
    %
    % This predicate reads the input stream until reaching one of
    % an end-of-term token, end-of-file, or I/O error.
    %
    % - If it finds no non-whitespace characters before the end-of-file,
    %   then it returns `eof'.
    %
    % - If it finds a sequence of tokens ending with an end-of-term token,
    %   which is a `.' followed by whitespace, then it leaves the trailing
    %   whitespace in the input stream, and decides what to do based on
    %   the contents of the token sequence before the end-of-term token.
    %
    %   - If the tokens form a syntactically correct ground term of the
    %     expected type, then it returns `ok(Term)'.
    %
    %   - If tokens do not form a syntactically correct term, or if the term
    %     they form is not ground, or if the term is not a valid term of the
    %     expected type, then it returns `error(Message, LineNumber)'.
    %
    % - If it encounters an I/O error, then it also returns
    %   `error(Message, LineNumber)'.
    %
:- pred read(io.read_result(T)::out, io::di, io::uo) is det.
:- pred read(io.text_input_stream::in, io.read_result(T)::out,
    io::di, io::uo) is det.

    % The type `posn' represents a position within a string.
    %
:- type posn
    --->    posn(int, int, int).
            % line number, offset of start of line, current offset (the first
            % two are used only for the purposes of computing term_contexts,
            % for use e.g. in error messages). Offsets start at zero.

    % read_from_string(FileName, String, MaxPos, Result, Posn0, Posn):
    % Same as read/4 except that it reads from a string rather than from a
    % stream.
    % FileName is the name of the source (for use in error messages).
    % String is the string to be parsed.
    % Posn0 is the position to start parsing from.
    % Posn is the position one past where the term read in ends.
    % MaxPos is the offset in the string which should be considered the
    % end-of-stream -- this is the upper bound for Posn. (In the usual case,
    % MaxPos is just the length of the String.)
    % WARNING: if MaxPos > length of String then the behaviour is UNDEFINED.
    %
:- pred read_from_string(string::in, string::in, int::in,
    read_result(T)::out, posn::in, posn::out) is det.

    % Discards all the whitespace from the current stream
    % or from the specified stream.
    %
:- pred ignore_whitespace(io.result::out, io::di, io::uo) is det.
:- pred ignore_whitespace(io.text_input_stream::in, io.result::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Text output predicates.
%

% These will all throw an io.error exception if an I/O error occurs.

    % print/3 writes its argument to the standard output stream.
    % print/4 writes its second argument to the output stream specified in
    % its first argument. In all cases, the argument to output can be of any
    % type. It is output in a format that is intended to be human readable.
    %
    % If the argument is just a single string or character, it will be printed
    % out exactly as is (unquoted). If the argument is of type integer (i.e.
    % an arbitrary precision integer), then its decimal representation will be
    % printed. If the argument is of type univ, then the value stored in the
    % the univ will be printed out, but not the type. If the argument is of
    % type date_time, it will be printed out in the same form as the string
    % returned by the function date_to_string/1. If the argument is of type
    % duration, it will be printed out in the same form as the string
    % returned by the function duration_to_string/1.
    %
    % print/5 is the same as print/4 except that it allows the caller to
    % specify how non-canonical types should be handled. print/3 and
    % print/4 implicitly specify `canonicalize' as the method for handling
    % non-canonical types. This means that for higher-order types, or types
    % with user-defined equality axioms, or types defined using the foreign
    % language interface (i.e. pragma foreign_type), the text output will only
    % describe the type that is being printed, not the value.
    %
    % print_cc/3 is the same as print/3 except that it specifies
    % `include_details_cc' rather than `canonicalize'. This means that it will
    % print the details of non-canonical types. However, it has determinism
    % `cc_multi'.
    %
    % Note that even if `include_details_cc' is specified, some implementations
    % may not be able to print all the details for higher-order types or types
    % defined using the foreign language interface.
    %
:- pred print(T::in, io::di, io::uo) is det.

:- pred print(io.text_output_stream::in, T::in, io::di, io::uo) is det.

:- pred print(io.text_output_stream, deconstruct.noncanon_handling, T, io, io).
:- mode print(in, in(do_not_allow), in, di, uo) is det.
:- mode print(in, in(canonicalize), in, di, uo) is det.
:- mode print(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode print(in, in, in, di, uo) is cc_multi.

:- pred print_cc(T::in, io::di, io::uo) is cc_multi.

    % print_line calls print and then writes a newline character.
    %
:- pred print_line(T::in, io::di, io::uo) is det.

:- pred print_line(io.text_output_stream::in, T::in, io::di, io::uo) is det.

:- pred print_line(io.text_output_stream, deconstruct.noncanon_handling,
    T, io, io).
:- mode print_line(in, in(do_not_allow), in, di, uo) is det.
:- mode print_line(in, in(canonicalize), in, di, uo) is det.
:- mode print_line(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode print_line(in, in, in, di, uo) is cc_multi.

:- pred print_line_cc(T::in, io::di, io::uo) is cc_multi.

    % write/3 writes its argument to the current output stream.
    % write/4 writes its second argument to the output stream specified
    % in its first argument. In all cases, the argument to output may be
    % of any type. The argument is written in a format that is intended to
    % be valid Mercury syntax whenever possible.
    %
    % Strings and characters are always printed out in quotes, using backslash
    % escapes if necessary. For higher-order types, or for types defined using
    % the foreign language interface (pragma foreign_type), the text output
    % will only describe the type that is being printed, not the value, and the
    % result may not be parsable by `read'. For the types containing
    % existential quantifiers, the type `type_desc' and closure types, the
    % result may not be parsable by `read', either. But in all other cases
    % the format used is standard Mercury syntax, and if you append a period
    % and newline (".\n"), then the results can be read in again using `read'.
    %
    % write/5 is the same as write/4 except that it allows the caller
    % to specify how non-canonical types should be handled. write_cc/3
    % is the same as write/3 except that it specifies `include_details_cc'
    % rather than `canonicalize'.
    %
:- pred write(T::in, io::di, io::uo) is det.
:- pred write(io.text_output_stream::in, T::in, io::di, io::uo) is det.

:- pred write(io.text_output_stream, deconstruct.noncanon_handling, T, io, io).
:- mode write(in, in(do_not_allow), in, di, uo) is det.
:- mode write(in, in(canonicalize), in, di, uo) is det.
:- mode write(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write(in, in, in, di, uo) is cc_multi.

:- pred write_line(io.text_output_stream, deconstruct.noncanon_handling, T,
    io, io).
:- mode write_line(in, in(do_not_allow), in, di, uo) is det.
:- mode write_line(in, in(canonicalize), in, di, uo) is det.
:- mode write_line(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write_line(in, in, in, di, uo) is cc_multi.

:- pred write_cc(T::in, io::di, io::uo) is cc_multi.
:- pred write_cc(io.text_output_stream::in, T::in, io::di, io::uo) is cc_multi.

    % write_line calls write and then writes a newline character.
    %
:- pred write_line(T::in, io::di, io::uo) is det.
:- pred write_line(io.text_output_stream::in, T::in, io::di, io::uo) is det.

:- pred write_line_cc(T::in, io::di, io::uo) is cc_multi.

    % Writes a newline character to the current output stream
    % or to the specified stream.
    %
:- pred nl(io::di, io::uo) is det.
:- pred nl(io.text_output_stream::in, io::di, io::uo) is det.

    % Writes a string to the current output stream or to the
    % specified output stream.
    %
:- pred write_string(string::in, io::di, io::uo) is det.
:- pred write_string(io.text_output_stream::in, string::in, io::di, io::uo)
    is det.

    % Writes a list of strings to the current output stream
    % or to the specified output stream.
    %
:- pred write_strings(list(string)::in, io::di, io::uo) is det.
:- pred write_strings(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

    % Writes a character to the current output stream
    % or to the specified output stream.
    %
:- pred write_char(char::in, io::di, io::uo) is det.
:- pred write_char(io.text_output_stream::in, char::in, io::di, io::uo)
    is det.

    % Writes an integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int(int::in, io::di, io::uo) is det.
:- pred write_int(io.text_output_stream::in, int::in, io::di, io::uo) is det.

    % Write a signed 8-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int8(int8::in, io::di, io::uo) is det.
:- pred write_int8(io.text_output_stream::in, int8::in, io::di, io::uo) is det.

    % Write a signed 16-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int16(int16::in, io::di, io::uo) is det.
:- pred write_int16(io.text_output_stream::in, int16::in, io::di, io::uo) is det.

    % Write a signed 32-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int32(int32::in, io::di, io::uo) is det.
:- pred write_int32(io.text_output_stream::in, int32::in, io::di, io::uo) is det.

    % Writes an unsigned integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_uint(uint::in, io::di, io::uo) is det.
:- pred write_uint(io.text_output_stream::in, uint::in, io::di, io::uo) is det.

    % Write an unsigned 8-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_uint8(uint8::in, io::di, io::uo) is det.
:- pred write_uint8(io.text_output_stream::in, uint8::in, io::di, io::uo) is det.

    % Write an unsigned 16-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_uint16(uint16::in, io::di, io::uo) is det.
:- pred write_uint16(io.text_output_stream::in, uint16::in, io::di, io::uo) is det.

    % Write an unsigned 32-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_uint32(uint32::in, io::di, io::uo) is det.
:- pred write_uint32(io.text_output_stream::in, uint32::in, io::di, io::uo) is det.

    % Writes a floating point number to the current output stream
    % or to the specified output stream.
    %
:- pred write_float(float::in, io::di, io::uo) is det.
:- pred write_float(io.text_output_stream::in, float::in, io::di, io::uo)
    is det.

    % Formats the specified arguments according to the format string,
    % using string.format, and then writes the result to the current
    % output stream or to the specified output stream.
    % (See the documentation of string.format for details.)
    %
:- pred format(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pred format(io.text_output_stream::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

    % Writes the specified arguments to the current output stream
    % or to the specified output stream.
    %
:- pred write_many(list(poly_type)::in, io::di, io::uo) is det.
:- pred write_many(io.text_output_stream::in, list(poly_type)::in,
    io::di, io::uo) is det.

    % write_list(List, Separator, OutputPred, !IO):
    % write_list(Stream, List, Separator, OutputPred, !IO):
    %
    % Applies OutputPred to each element of List, printing Separator
    % (to the current output stream or to Stream) between each element.
    %
:- pred write_list(list(T), string, pred(T, io, io), io, io).
:- mode write_list(in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode write_list(in, in, pred(in, di, uo) is cc_multi, di, uo)
    is cc_multi.

    % write_list(Stream, List, Separator, OutputPred, !IO):
    % Sets the current output stream to Stream, then applies OutputPred to
    % each element of List, printing Separator between each element.
    % The original output stream is restored whether returning normally
    % or if an exception is thrown.
    %
:- pred write_list(io.text_output_stream, list(T), string,
    pred(T, io, io), io, io).
:- mode write_list(in, in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode write_list(in, in, in, pred(in, di, uo) is cc_multi, di, uo)
    is cc_multi.

    % write_array(Array, Separator, OutputPred, !IO):
    % Applies OutputPred to each element of Array, printing Separator
    % to the current output stream between each element.
    %
:- pred write_array(array(T), string, pred(T, io, io), io, io).
:- mode write_array(in, in, pred(in, di, uo) is det, di, uo) is det.
%:- mode write_array(array_ui, in, pred(in, di, uo) is det, di uo) is det.
:- mode write_array(in, in, pred(in, di, uo) is cc_multi, di, uo) is cc_multi.
%:- mode write_array(array_ui, in, pred(in, di, uo) is cc_multi, di uo)
% is cc_multi.

    % write_array(Stream, Array, Separator, OutputPred, !IO):
    % Sets the current output stream to Stream, then applies OutputPred to
    % each element of Array, printing Separator between each element.
    % The original output stream is restored whether returning normally
    % or if an exception is thrown.
    %
:- pred write_array(io.text_output_stream, array(T), string, pred(T, io, io),
    io, io).
:- mode write_array(in, in, in, pred(in, di, uo) is det, di, uo) is det.
%:- mode write_array(in, array_ui, in, pred(in, di, uo) is det, di uo) is det.
:- mode write_array(in, in, in, pred(in, di, uo) is cc_multi, di, uo)
    is cc_multi.
%:- mode write_array(in, array_ui, in, pred(in, di, uo) is cc_multi, di uo)
% is cc_multi.

    % Flush the output buffer of the current output stream
    % or to the specified output stream.
    %
:- pred flush_output(io::di, io::uo) is det.
:- pred flush_output(io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Input text stream predicates.
%

    % see(File, Result, !IO):
    % Attempts to open a file for input, and if successful, sets the current
    % input stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrorCode)'.
    %
:- pred see(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current input stream.
    % The current input stream reverts to standard input.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred seen(io::di, io::uo) is det.

    % Attempts to open a file for input.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_input(string::in, io.res(io.text_input_stream)::out,
    io::di, io::uo) is det.

    % Closes an open input stream.
    % Throw an io.error exception if an I/O error occurs.
    %
:- pred close_input(io.text_input_stream::in, io::di, io::uo) is det.

    % Retrieves the current input stream.
    % Does not modify the I/O state.
    %
:- pred input_stream(io.text_input_stream::out, io::di, io::uo) is det.

    % set_input_stream(NewStream, OldStream, !IO):
    % Changes the current input stream to the stream specified.
    % Returns the previous stream.
    %
:- pred set_input_stream(io.text_input_stream::in, io.text_input_stream::out,
    io::di, io::uo) is det.

    % Retrieves the standard input stream.
    %
:- func stdin_stream = io.text_input_stream.

    % Retrieves the standard input stream.
    % Does not modify the I/O state.
    %
:- pred stdin_stream(io.text_input_stream::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current input
    % stream. For file streams, this is the filename. For stdin,
    % this is the string "<standard input>".
    %
:- pred input_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified input
    % stream. For file streams, this is the filename. For stdin,
    % this is the string "<standard input>".
    %
:- pred input_stream_name(io.text_input_stream::in, string::out,
    io::di, io::uo) is det.

    % Return the line number of the current input stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % set_line_number.
    %
:- pred get_line_number(int::out, io::di, io::uo) is det.

    % Return the line number of the specified input stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % set_line_number.
    %
:- pred get_line_number(io.text_input_stream::in, int::out, io::di, io::uo)
    is det.

    % Set the line number of the current input stream.
    %
:- pred set_line_number(int::in, io::di, io::uo) is det.

    % Set the line number of the specified input stream.
    %
:- pred set_line_number(io.text_input_stream::in, int::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Output text stream predicates.
%

    % Attempts to open a file for output, and if successful sets the current
    % output stream to the newly opened stream. As per Prolog tell/1.
    % Result is either 'ok' or 'error(ErrCode)'.
    %
:- pred tell(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current output stream; the default output stream reverts
    % to standard output. As per Prolog told/0. This will throw an
    % io.error exception if an I/O error occurs.
    %
:- pred told(io::di, io::uo) is det.

    % Attempts to open a file for output.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_output(string::in, io.res(io.text_output_stream)::out,
    io::di, io::uo) is det.

    % Attempts to open a file for appending.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_append(string::in, io.res(io.text_output_stream)::out,
    io::di, io::uo) is det.

    % Closes an open output stream.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred close_output(io.text_output_stream::in, io::di, io::uo) is det.

    % Retrieves the current output stream.
    % Does not modify the I/O state.
    %
:- pred output_stream(io.text_output_stream::out, io::di, io::uo) is det.

    % Changes the current output stream to the stream specified.
    % Returns the previous stream.
    %
:- pred set_output_stream(io.text_output_stream::in,
    io.text_output_stream::out, io::di, io::uo) is det.

    % Retrieves the standard output stream.
    %
:- func stdout_stream = io.text_output_stream.

    % Retrieves the standard output stream.
    % Does not modify the I/O state.
    %
:- pred stdout_stream(io.text_output_stream::out, io::di, io::uo) is det.

    % Retrieves the standard error stream.
    %
:- func stderr_stream = io.text_output_stream.

    % Retrieves the standard error stream.
    % Does not modify the I/O state.
    %
:- pred stderr_stream(io.text_output_stream::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current
    % output stream.
    % For file streams, this is the filename.
    % For stdout this is the string "<standard output>".
    % For stderr this is the string "<standard error>".
    %
:- pred output_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified stream.
    % For file streams, this is the filename.
    % For stdout this is the string "<standard output>".
    % For stderr this is the string "<standard error>".
    %
:- pred output_stream_name(io.text_output_stream::in, string::out,
    io::di, io::uo) is det.

    % Return the line number of the current output stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % set_output_line_number.
    %
:- pred get_output_line_number(int::out, io::di, io::uo) is det.

    % Return the line number of the specified output stream. Lines are normally
    % numbered starting at 1, but this can be overridden by calling
    % set_output_line_number.
    %
:- pred get_output_line_number(io.text_output_stream::in, int::out,
    io::di, io::uo) is det.

    % Set the line number of the current output stream.
    %
:- pred set_output_line_number(int::in, io::di, io::uo) is det.

    % Set the line number of the specified output stream.
    %
:- pred set_output_line_number(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Binary input predicates.
%

    % Reads a binary representation of a term of type T from the current
    % binary input stream or from the specified binary input stream.
    %
    % Note: if you attempt to read a binary representation written by
    % a different program, or a different version of the same program,
    % then the results are not guaranteed to be meaningful. Another caveat
    % is that higher-order types cannot be read. (If you try, you will get
    % a runtime error.)
    %
    % XXX Note also that due to the current implementation,
    % read_binary will not work for the Java back-end.
    %
:- pred read_binary(io.result(T)::out, io::di, io::uo) is det.
:- pred read_binary(io.binary_input_stream::in, io.result(T)::out,
    io::di, io::uo) is det.

    % Reads a single 8-bit byte from the current binary input stream
    % or from the specified binary input stream.
    %
:- pred read_byte(io.result(int)::out, io::di, io::uo) is det.
:- pred read_byte(io.binary_input_stream::in, io.result(int)::out,
    io::di, io::uo) is det.

    % Reads a single signed 8-bit integer from the current binary input
    % stream or from the specified binary input stream.
    %
:- pred read_binary_int8(io.result(int8)::out, io::di, io::uo) is det.
:- pred read_binary_int8(io.binary_input_stream::in, io.result(int8)::out,
    io::di, io::uo) is det.

    % Reads a single unsigned 8-bit integer from the current binary input
    % stream or from the specified binary input stream.
    %
:- pred read_binary_uint8(io.result(uint8)::out, io::di, io::uo) is det.
:- pred read_binary_uint8(io.binary_input_stream::in, io.result(uint8)::out,
    io::di, io::uo) is det.

    % Fill a bitmap from the current binary input stream
    % or from the specified binary input stream.
    % Return the number of bytes read. On end-of-file, the number of
    % bytes read will be less than the size of the bitmap, and
    % the result will be `ok'.
    %
:- pred read_bitmap(bitmap::bitmap_di, bitmap::bitmap_uo,
    int::out, io.res::out, io::di, io::uo) is det.
:- pred read_bitmap(io.binary_input_stream::in,
    bitmap::bitmap_di, bitmap::bitmap_uo,
    int::out, io.res::out, io::di, io::uo) is det.

    % read_bitmap(StartByte, NumBytes, !Bitmap, BytesRead, Result, !IO)
    %
    % Read NumBytes bytes into a bitmap starting at StartByte from the
    % current binary input stream, or from the specified binary input stream.
    % Return the number of bytes read. On end-of-file, the number of
    % bytes read will be less than NumBytes, and the result will be `ok'.
    %
:- pred read_bitmap(byte_index::in, num_bytes::in,
    bitmap::bitmap_di, bitmap::bitmap_uo, num_bytes::out, io.res::out,
    io::di, io::uo) is det.
:- pred read_bitmap(io.binary_input_stream::in, byte_index::in, num_bytes::in,
    bitmap::bitmap_di, bitmap::bitmap_uo, num_bytes::out, io.res::out,
    io::di, io::uo) is det.

    % Reads all the bytes until eof or error from the current binary input
    % stream or from the specified binary input stream into a bitmap.
    %
:- pred read_binary_file_as_bitmap(
    io.res(bitmap)::out, io::di, io::uo) is det.
:- pred read_binary_file_as_bitmap(io.binary_input_stream::in,
    io.res(bitmap)::out, io::di, io::uo) is det.

    % Reads all the bytes until eof or error from the current binary input
    % stream or from the specified binary input stream.
    %
:- pred read_binary_file(
    io.result(list(int))::out, io::di, io::uo) is det.
:- pred read_binary_file(io.binary_input_stream::in,
    io.result(list(int))::out, io::di, io::uo) is det.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl(pred(int, T, T),
    T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl((pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl((pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl_io(pred(int, io, io),
    io.res, io, io).
:- mode binary_input_stream_foldl_io((pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode binary_input_stream_foldl_io((pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl2_io(
    pred(int, T, T, io, io), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io(
    in(pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io(
    in(pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error, or the closure returns `no'
    % as its second argument.
    %
:- pred binary_input_stream_foldl2_io_maybe_stop(
    pred(int, bool, T, T, io, io), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_maybe_stop(
    (pred(in, out, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl(io.binary_input_stream,
    pred(int, T, T), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl_io(io.binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode binary_input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode binary_input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl2_io(io.binary_input_stream,
    pred(int, T, T, io, io), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Applies the given closure to each byte read from the given binary input
    % stream in turn, until eof or error, or the closure returns `no' as its
    % second argument.
    %
:- pred binary_input_stream_foldl2_io_maybe_stop(io.binary_input_stream,
    pred(int, bool, T, T, io, io), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_maybe_stop(in,
    (pred(in, out, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

    % Un-reads a byte from the current binary input stream
    % or from the specified stream.
    %
    % You can put back as many bytes as you like.
    % You can even put back something that you didn't actually read.
    %
    % On some systems and backends, only one byte of pushback is guaranteed.
    % `putback_byte' will throw an io.error exception if the pushback buffer
    % is full.
    %
    % Pushing back a byte decrements the file position by one, except when
    % the file position is already zero, in which case the new file position
    % is unspecified.
    %
:- pred putback_byte(int::in, io::di, io::uo) is det.
:- pred putback_byte(io.binary_input_stream::in, int::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Binary output predicates.
%

% These will all throw an io.error exception if an I/O error occurs.
% XXX what about wide characters?

    % Writes a binary representation of a term to the current binary output
    % stream or to the specified stream, in a format suitable for reading in
    % again with read_binary.
    %
:- pred write_binary(T::in, io::di, io::uo) is det.
:- pred write_binary(io.binary_output_stream::in, T::in, io::di, io::uo)
    is det.

    % Writes a single byte to the current binary output stream
    % or to the specified binary output stream. The byte is taken from
    % the bottom 8 bits of the specified int.
    %
:- pred write_byte(int::in, io::di, io::uo) is det.
:- pred write_byte(io.binary_output_stream::in, int::in, io::di, io::uo)
    is det.

    % Writes a signed 8-bit integer to the current binary output stream
    % or to the specified binary output stream.
    %
:- pred write_binary_int8(int8::in, io::di, io::uo) is det.
:- pred write_binary_int8(io.binary_output_stream::in, int8::in,
    io::di, io::uo) is det.

    % Writes an unsigned 8-bit integer to the current binary output stream
    % or to the specified binary output stream.
    %
:- pred write_binary_uint8(uint8::in, io::di, io::uo) is det.
:- pred write_binary_uint8(io.binary_output_stream::in, uint8::in,
    io::di, io::uo) is det.

    % Write a bitmap to the current binary output stream
    % or to the specified binary output stream. The bitmap must not contain
    % a partial final byte.
    %
:- pred write_bitmap(bitmap, io, io).
%:- mode write_bitmap(bitmap_ui, di, uo) is det.
:- mode write_bitmap(in, di, uo) is det.
:- pred write_bitmap(io.binary_output_stream, bitmap, io, io).
%:- mode write_bitmap(in, bitmap_ui, di, uo) is det.
:- mode write_bitmap(in, in, di, uo) is det.

    % write_bitmap(BM, StartByte, NumBytes, !IO):
    % write_bitmap(Stream, BM, StartByte, NumBytes, !IO):
    %
    % Write part of a bitmap to the current binary output stream
    % or to the specified binary output stream.
    %
:- pred write_bitmap(bitmap, int, int, io, io).
%:- mode write_bitmap(bitmap_ui, in, in, di, uo) is det.
:- mode write_bitmap(in, in, in, di, uo) is det.
:- pred write_bitmap(io.binary_output_stream, bitmap, int, int, io, io).
%:- mode write_bitmap(in, bitmap_ui, in, in, di, uo) is det.
:- mode write_bitmap(in, in, in, in, di, uo) is det.

    % Flush the output buffer of the current binary output stream.
    % or of the specified binary output stream.
    %
:- pred flush_binary_output(io::di, io::uo) is det.
:- pred flush_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary input stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
    % A successful seek undoes any effects of putback_byte on the stream.
    %
:- pred seek_binary_input(io.binary_input_stream::in, io.whence::in,
    int::in, io::di, io::uo) is det.

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary output stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
:- pred seek_binary_output(io.binary_output_stream::in, io.whence::in,
    int::in, io::di, io::uo) is det.

    % Returns the offset (in bytes) into the specified binary input stream.
    %
:- pred binary_input_stream_offset(io.binary_input_stream::in, int::out,
    io::di, io::uo) is det.

    % Returns the offset (in bytes) into the specified binary output stream.
    %
:- pred binary_output_stream_offset(io.binary_output_stream::in, int::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Binary input stream predicates.
%

    % Attempts to open a file for binary input, and if successful sets
    % the current binary input stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrorCode)'.
    %
:- pred see_binary(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current input stream. The current input stream reverts
    % to standard input. This will throw an io.error exception if
    % an I/O error occurs.
    %
:- pred seen_binary(io::di, io::uo) is det.

    % Attempts to open a binary file for input.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_binary_input(string::in,
    io.res(io.binary_input_stream)::out, io::di, io::uo) is det.

    % Closes an open binary input stream. This will throw an io.error
    % exception if an I/O error occurs.
    %
:- pred close_binary_input(io.binary_input_stream::in,
    io::di, io::uo) is det.

    % Retrieves the current binary input stream.
    % Does not modify the I/O state.
    %
:- pred binary_input_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

    % Changes the current input stream to the stream specified.
    % Returns the previous stream.
    %
:- pred set_binary_input_stream(io.binary_input_stream::in,
    io.binary_input_stream::out, io::di, io::uo) is det.

    % Retrieves the standard binary input stream.
    % Does not modify the I/O state.
    %
:- pred stdin_binary_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current binary
    % input stream. For file streams, this is the filename.
    %
:- pred binary_input_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified
    % binary input stream. For file streams, this is the filename.
    %
:- pred binary_input_stream_name(io.binary_input_stream::in, string::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Binary output stream predicates.
%

    % Attempts to open a file for binary output, and if successful sets
    % the current binary output stream to the newly opened stream.
    % As per Prolog tell/1. Result is either 'ok' or 'error(ErrCode)'.
    %
:- pred tell_binary(string::in, io.res::out, io::di, io::uo) is det.

    % Closes the current binary output stream. The default binary output
    % stream reverts to standard output. As per Prolog told/0. This will
    % throw an io.error exception if an I/O error occurs.
    %
:- pred told_binary(io::di, io::uo) is det.

    % Attempts to open a file for binary output.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_binary_output(string::in,
    io.res(io.binary_output_stream)::out, io::di, io::uo) is det.

    % Attempts to open a file for binary appending.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_binary_append(string::in,
    io.res(io.binary_output_stream)::out, io::di, io::uo) is det.

    % Closes an open binary output stream.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred close_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

    % Retrieves the current binary output stream.
    % Does not modify the I/O state.
    %
:- pred binary_output_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

    % Retrieves the standard binary output stream.
    % Does not modify the I/O state.
    %
:- pred stdout_binary_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

    % Changes the current binary output stream to the stream specified.
    % Returns the previous stream.
    %
:- pred set_binary_output_stream(io.binary_output_stream::in,
    io.binary_output_stream::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current
    % binary output stream. For file streams, this is the filename.
    %
:- pred binary_output_stream_name(string::out, io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the specified
    % output stream. For file streams, this is the filename.
    %
:- pred binary_output_stream_name(io.binary_output_stream::in,
    string::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Global state predicates.
%

    % progname(DefaultProgname, Progname).
    %
    % Returns the name that the program was invoked with, if available,
    % or DefaultProgname if the name is not available.
    % Does not modify the I/O state.
    %
:- pred progname(string::in, string::out, io::di, io::uo) is det.

    % progname_base(DefaultProgname, Progname).
    %
    % Like `progname', except that it strips off any path name
    % preceding the program name. Useful for error messages.
    %
:- pred progname_base(string::in, string::out, io::di, io::uo) is det.

    % Returns the arguments that the program was invoked with,
    % if available, otherwise an empty list. Does not modify the I/O state.
    %
:- pred command_line_arguments(list(string)::out, io::di, io::uo) is det.

    % The I/O state contains an integer used to record the program's exit
    % status. When the program finishes, it will return this exit status
    % to the operating system. The following predicates can be used to get
    % and set the exit status.
    %
:- pred get_exit_status(int::out, io::di, io::uo) is det.
:- pred set_exit_status(int::in, io::di, io::uo) is det.

    % The I/O state includes a `globals' field which is not used by the
    % standard library, but can be used by the application. The globals field
    % is of type `univ' so that the application can store any data it wants
    % there. The following predicates can be used to access this global state.
    %
    % Does not modify the I/O state.
    %
:- pred get_globals(univ::out, io::di, io::uo) is det.
:- pred set_globals(univ::in, io::di, io::uo) is det.

    % update_globals(UpdatePred, !IO).
    % Update the `globals' field in the I/O state based upon its current value.
    % This is equivalent to the following:
    %
    %   get_globals(Globals0, !IO),
    %   UpdatePred(Globals0, Globals),
    %   set_globals(Globals, !IO)
    %
    % In parallel grades calls to update_globals/3 are atomic.
    % If `UpdatePred' throws an exception then the `globals' field is
    % left unchanged.
    %
:- pred update_globals(pred(univ, univ)::in(pred(in, out) is det),
    io::di, io::uo) is det.

    % The following predicates provide an interface to the environment list.
    % Do not attempt to put spaces or '=' signs in the names of environment
    % variables, or bad things may result!
    %
    % First argument is the name of the environment variable. Returns
    % yes(Value) if the variable was set (Value will be set to the value
    % of the variable) and no if the variable was not set.
    %
:- pred get_environment_var(string::in, maybe(string)::out,
    io::di, io::uo) is det.

    % First argument is the name of the environment variable, second argument
    % is the value to be assigned to that variable. Res is 'ok' on success or
    % 'error(ErrorCode)' if the system runs out of environment space or if
    % the environment cannot be modified.
    %
    % Note that the environment cannot be modified on Java.
    %
:- pred set_environment_var(string::in, string::in, io.res::out,
    io::di, io::uo) is det.

    % Same as set_environment_var/5, but throws an exception if an error
    % occurs.
    %
:- pred set_environment_var(string::in, string::in, io::di, io::uo) is det.

    % Test if the set_environment_var/{4,5} predicates are available.  This
    % is false for Java backends.
    %
:- pred have_set_environment_var is semidet.

%---------------------------------------------------------------------------%
%
% File handling predicates.
%

    % make_temp_file(Result, !IO) creates an empty file whose name is different
    % to the name of any existing file.  If successful Result returns the name
    % of the file.  It is the responsibility of the caller to delete the file
    % when it is no longer required.
    %
    % The file is placed in the directory returned by get_temp_directory/3.
    %
    % On the Erlang and Java backends, this does not attempt to create the file
    % with restrictive permissions (600 on Unix-like systems) and therefore
    % should not be used when security is required.
    %
:- pred make_temp_file(io.res(string)::out, io::di, io::uo) is det.

    % Like make_temp_file/3 except it throws an io.error exception if the
    % temporary file could not be created.
    %
:- pragma obsolete(make_temp/3).
:- pred make_temp(string::out, io::di, io::uo) is det.

    % make_temp_file(Dir, Prefix, Suffix, Result, !IO) creates an empty file
    % whose name is different to the name of any existing file. The file will
    % reside in the directory specified by Dir and will have a prefix using up
    % to the first 5 characters of Prefix. If successful, Result returns the
    % name of the file. It is the responsibility of the caller to delete the
    % file when it is no longer required.
    %
    % The C backend has the following limitations:
    %   - Suffix may be ignored.
    %
    % The C# backend has the following limitations:
    %   - Dir is ignored.
    %   - Prefix is ignored.
    %   - Suffix is ignored.
    %
    % On the Erlang backend Suffix is ignored.
    %
    % On the Erlang and Java backends, this does not attempt to create the file
    % with restrictive permissions (600 on Unix-like systems) and therefore
    % should not be used when security is required.
    %
:- pred make_temp_file(string::in, string::in, string::in, io.res(string)::out,
    io::di, io::uo) is det.

    % Same as make_temp_file except it does not take a suffix argument and
    % throws an io.error exception if the temporary file could not be created.
    %
:- pragma obsolete(make_temp/5).
:- pred make_temp(string::in, string::in, string::out, io::di, io::uo) is det.

    % make_temp_directory(Result, !IO) creates an empty directory whose name
    % is different from the name of any existing directory.
    %
    % On the Java backend this is insecure as the file permissions are not set.
    %
    % This is unimplemented on the Erlang backend.
    %
:- pred make_temp_directory(io.res(string)::out, io::di, io::uo) is det.

    % make_temp_directory(Dir, Prefix, Suffix, Result, !IO) creates an empty
    % directory whose name is different from the name of any existing
    % directory.  The new directory will reside in the existing directory
    % specified by Dir and will have a prefix using up to the first 5
    % characters of Prefix and a Suffix.  Result returns the name of the
    % new directory. It is the responsibility of the program to delete the
    % directory when it is no longer needed.
    %
    % The C backend has the following limitations:
    %   - Suffix is ignored.
    %
    % The C# backend has the following limitations:
    %   - Prefix is ignored.
    %   - Suffix is ignored.
    %
    % On the Java backend this is insecure as the file permissions are not set.
    %
    % This is unimplemented on the Erlang backend.
    %
:- pred make_temp_directory(string::in, string::in, string::in,
    io.res(string)::out, io::di, io::uo) is det.

    % Test if the make_temp_directory predicates are available.  This is false
    % for the Erlang backends and either C backend without support for
    % mkdtemp(3).
    %
:- pred have_make_temp_directory is semidet.

    % get_temp_directory(DirName, !IO)
    %
    % DirName is the name of a directory where applications should put
    % temporary files.
    %
    % This is implementation-dependent.  For current Mercury implementations,
    % it is determined as follows:
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
:- pred get_temp_directory(string::out, io::di, io::uo) is det.

    % remove_file(FileName, Result, !IO) attempts to remove the file
    % `FileName', binding Result to ok/0 if it succeeds, or error/1 if it
    % fails. If `FileName' names a file that is currently open, the behaviour
    % is implementation-dependent.
    %
:- pred remove_file(string::in, io.res::out, io::di, io::uo) is det.

    % remove_file_recursively(FileName, Result, !IO) attempts to remove
    % the file `FileName', binding Result to ok/0 if it succeeds, or error/1
    % if it fails. If `FileName' names a file that is currently open, the
    % behaviour is implementation-dependent.
    %
    % Unlike `remove_file', this predicate will attempt to remove non-empty
    % directories (recursively). If it fails, some of the directory elements
    % may already have been removed.
    %
:- pred remove_file_recursively(string::in, io.res::out, io::di, io::uo)
    is det.

    % rename_file(OldFileName, NewFileName, Result, !IO).
    %
    % Attempts to rename the file `OldFileName' as `NewFileName', binding
    % Result to ok/0 if it succeeds, or error/1 if it fails. If `OldFileName'
    % names a file that is currently open, the behaviour is
    % implementation-dependent. If `NewFileName' names a file that already
    % exists the behaviour is also implementation-dependent; on some systems,
    % the file previously named `NewFileName' will be deleted and replaced
    % with the file previously named `OldFileName'.
    %
:- pred rename_file(string::in, string::in, io.res::out, io::di, io::uo)
    is det.

    % Succeeds if this platform can read and create symbolic links.
    %
:- pred have_symlinks is semidet.

    % make_symlink(FileName, LinkFileName, Result, !IO).
    %
    % Attempts to make `LinkFileName' be a symbolic link to `FileName'.
    % If `FileName' is a relative path, it is interpreted relative
    % to the directory containing `LinkFileName'.
    %
:- pred make_symlink(string::in, string::in, io.res::out, io::di, io::uo)
    is det.

    % read_symlink(FileName, Result, !IO) returns `ok(LinkTarget)'
    % if `FileName' is a symbolic link pointing to `LinkTarget', and
    % `error(Error)' otherwise. If `LinkTarget' is a relative path,
    % it should be interpreted relative the directory containing `FileName',
    % not the current directory.
    %
:- pred read_symlink(string::in, io.res(string)::out, io::di, io::uo)
    is det.

:- type access_type
    --->    read
    ;       write
    ;       execute.

    % check_file_accessibility(FileName, AccessTypes, Result):
    %
    % Check whether the current process can perform the operations given
    % in `AccessTypes' on `FileName'.
    % XXX When using the .NET CLI, this predicate will sometimes report
    % that a directory is writable when in fact it is not.
    % XXX On the Erlang backend, or on Windows with some compilers, `execute'
    % access is not checked.
    %
:- pred check_file_accessibility(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.

:- type file_type
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

    % file_type(FollowSymLinks, FileName, TypeResult)
    % finds the type of the given file.
    %
:- pred file_type(bool::in, string::in, io.res(file_type)::out,
    io::di, io::uo) is det.

    % file_modification_time(FileName, TimeResult)
    % finds the last modification time of the given file.
    %
:- pred file_modification_time(string::in, io.res(time_t)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Memory management predicates.
%

    % Write memory/time usage statistics to stderr.
    %
:- pred report_stats(io::di, io::uo) is det.

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
:- pred report_stats(string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Miscellaneous predicates.
%

    % Invokes the operating system shell with the specified Command.
    % Result is either `ok(ExitStatus)', if it was possible to invoke
    % the command, or `error(ErrorCode)' if not. The ExitStatus will be 0
    % if the command completed successfully or the return value of the system
    % call. If a signal kills the system call, then Result will be an error
    % indicating which signal occurred.
    %
:- pred call_system(string::in, io.res(int)::out, io::di, io::uo) is det.

:- type system_result
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
:- pred call_system_return_signal(string::in,
    io.res(system_result)::out, io::di, io::uo) is det.

    % Construct an error code including the specified error message.
    %
:- func make_io_error(string) = io.error.

    % Look up the error message corresponding to a particular error code.
    %
:- func error_message(io.error) = string.
:- pred error_message(io.error::in, string::out) is det.

%---------------------------------------------------------------------------%
%
% Instances of the stream typeclass.
%

:- instance stream.error(io.error).

:- instance stream.stream(text_output_stream, io).
:- instance stream.output(text_output_stream, io).
:- instance stream.writer(text_output_stream, char,   io).
:- instance stream.writer(text_output_stream, float,  io).
:- instance stream.writer(text_output_stream, int,    io).
:- instance stream.writer(text_output_stream, int8,   io).
:- instance stream.writer(text_output_stream, int16,  io).
:- instance stream.writer(text_output_stream, int32,  io).
:- instance stream.writer(text_output_stream, uint,   io).
:- instance stream.writer(text_output_stream, uint8,  io).
:- instance stream.writer(text_output_stream, uint16, io).
:- instance stream.writer(text_output_stream, uint8,  io).
:- instance stream.writer(text_output_stream, string, io).
:- instance stream.writer(text_output_stream, univ,   io).
:- instance stream.line_oriented(text_output_stream, io).

:- instance stream.stream(text_input_stream, io).
:- instance stream.input(text_input_stream, io).
:- instance stream.reader(text_input_stream, char, io, io.error).
:- instance stream.reader(text_input_stream, line, io, io.error).
:- instance stream.reader(text_input_stream, text_file, io, io.error).

:- instance stream.line_oriented(text_input_stream, io).
:- instance stream.putback(text_input_stream, char, io, io.error).

:- instance stream.stream(binary_output_stream, io).
:- instance stream.output(binary_output_stream, io).
:- instance stream.writer(binary_output_stream, byte, io).
:- instance stream.writer(binary_output_stream, int8, io).
:- instance stream.writer(binary_output_stream, uint8, io).
:- instance stream.writer(binary_output_stream, bitmap.slice, io).
:- instance stream.seekable(binary_output_stream, io).

:- instance stream.stream(binary_input_stream,  io).
:- instance stream.input(binary_input_stream, io).
:- instance stream.reader(binary_input_stream, int, io, io.error).
:- instance stream.bulk_reader(binary_input_stream, int,
        bitmap, io, io.error).
:- instance stream.putback(binary_input_stream, int, io, io.error).
:- instance stream.seekable(binary_input_stream, io).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%---------------------------------------------------------------------------%

:- interface.

%
% For use by library.m:
%

:- pred init_state(io::di, io::uo) is det.
:- pred finalize_state(io::di, io::uo) is det.

%
% For use by dir.m:
%

    % Succeeds iff the Win32 API is available.
    %
:- pred have_win32 is semidet.

    % Succeeds iff the current process was compiled against the Cygwin library.
    %
:- pred have_cygwin is semidet.

    % Succeeds iff the .NET class library is available.
    %
:- pred have_dotnet is semidet.

    % A system-dependent error indication.
    % For C, this is 0 for success or the value of errno.
    % (In a few cases, we pass a Win32 error as a system_error.)
    % For Java, this is null for success or an exception object.
    % For C#, this is null for success or an exception object.
    % For Erlang, this is `ok' for success or `{error, Reason}'.
    %
:- type system_error.
:- pragma foreign_type(c, system_error, "MR_Integer").
:- pragma foreign_type("C#", system_error, "System.Exception").
:- pragma foreign_type(java, system_error, "java.lang.Exception").
:- pragma foreign_type(erlang, system_error, "").

    % is_error(Error, MessagePrefix, IOError):
    % Succeeds iff `Error' indicates an error (not success).
    % IOError contains an error message obtained by looking up the
    % message for the given errno value and prepending
    % `MessagePrefix'.
    %
:- pred is_error(system_error::in, string::in, io.error::out) is semidet.

    % is_maybe_win32_error(Error, MessagePrefix, IOError):
    % Same as is_error except that `Error' is a Win32 error value on Windows.
    %
:- pred is_maybe_win32_error(system_error::in, string::in, io.error::out)
    is semidet.

    % make_err_msg(Error, MessagePrefix, Message):
    % `Message' is an error message obtained by looking up the
    % message for the given errno value and prepending
    % `MessagePrefix'.
    %
:- pred make_err_msg(system_error::in, string::in, string::out) is det.

    % make_maybe_win32_err_msg(Error, MessagePrefix, Message):
    %
    % `Message' is an error message obtained by looking up the error message
    % for `Error' and prepending `MessagePrefix'.
    % On Win32 systems, `Error' is obtained by calling GetLastError.
    % On other systems `Error' is obtained by reading errno.
    %
:- pred make_maybe_win32_err_msg(system_error::in, string::in, string::out)
    is det.

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
:- pred file_id(string::in, io.res(file_id)::out, io::di, io::uo) is det.

%
% For use by term_io.m:
%

:- import_module ops.

:- pred get_op_table(ops.table::out, io::di, io::uo) is det.

:- pred set_op_table(ops.table::di, io::di, io::uo) is det.

%
% For use by browser/browse.m:
%

% Types and predicates for managing the stream info database.

:- type stream_db ==    map(stream_id, stream_info).

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
:- pred get_stream_db(stream_db::out, io::di, io::uo) is det.
:- impure pred get_stream_db_with_locking(stream_db::out) is det.

    % Returns the information associated with the specified input
    % stream in the given stream database.
    %
:- func input_stream_info(stream_db, io.text_input_stream)
    = maybe_stream_info.

    % Returns the information associated with the specified output
    % stream in the given stream database.
    %
:- func output_stream_info(stream_db, io.text_output_stream)
    = maybe_stream_info.

    % Returns the information associated with the specified binary input
    % stream in the given stream database.
    %
:- func binary_input_stream_info(stream_db, io.binary_input_stream)
    = maybe_stream_info.

    % Returns the information associated with the specified binary output
    % stream in the given stream database.
    %
:- func binary_output_stream_info(stream_db, io.binary_output_stream)
    = maybe_stream_info.

    % If the univ contains an I/O stream, return information about that
    % stream, otherwise fail.
:- func get_io_stream_info(stream_db, T) = maybe_stream_info is semidet.

%
% For use by compiler/process_util.m:
%

    % Interpret the child process exit status returned by
    % system() or wait().
    %
:- func decode_system_command_exit_code(int) = io.res(io.system_result).

%
% For use by the compiler transformation that implements trace [io(!IO)]:
%

:- semipure pred unsafe_get_io_state(io::uo) is det.

:- impure pred unsafe_set_io_state(io::di) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module dir.
:- import_module exception.
:- import_module int.
:- import_module int8.
:- import_module parser.
:- import_module require.
:- import_module stream.string_writer.
:- import_module term.
:- import_module term_conversion.
:- import_module term_io.
:- import_module type_desc.
:- import_module uint8.

:- use_module rtti_implementation.
:- use_module table_builtin.

%---------------------------------------------------------------------------%

:- pragma foreign_import_module("C", time).   % For ML_construct_time_t.
:- pragma foreign_import_module("C", string).

    % Values of type `io.state' are never really used:
    % instead we store data in global variables.
    % The reason this is defined as a foreign type is to prevent attempts
    % to deconstruct values of the type.
:- pragma foreign_type("C", io.state, "MR_Word", [can_pass_as_mercury_type]).
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

:- pragma foreign_decl("C#", "
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.AccessControl;
using System.Security.Principal;
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
    // by an environment variable. (This might require moving
    // the code which initializes mercury_stdin, etc.)
    //
    static readonly ML_line_ending_kind ML_default_line_ending =
        ML_line_ending_kind.ML_OS_line_ending;

    // Assume UTF-8 encoding on files. When writing a file, don't emit
    // a byte order mark.
    static readonly System.Text.Encoding text_encoding =
        new System.Text.UTF8Encoding(false);

#if __MonoCS__
    // int chmod(const char *path, mode_t mode);
    [DllImport(""libc"", SetLastError=true, EntryPoint=""mkdir"",
        CallingConvention=CallingConvention.Cdecl)]
    static extern int ML_sys_mkdir (string path, uint mode);
#endif
").

:- pragma foreign_code("Java",
"
    public static tree234.Tree234_2<Integer, Stream_info_0> ML_io_stream_db
        = new tree234.Tree234_2.Empty_0<Integer, Stream_info_0>();
    public static univ.Univ_0 ML_io_user_globals = null;
").

:- type input_stream  ---> input_stream(stream).
:- type output_stream ---> output_stream(stream).

:- type binary_input_stream ---> binary_input_stream(stream).
:- type binary_output_stream ---> binary_output_stream(stream).

:- type stream
    --->    stream(c_pointer).
:- pragma foreign_type("C", stream, "MercuryFilePtr",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", stream, "io.MR_MercuryFileStruct").
:- pragma foreign_type("Java", stream, "io.MR_MercuryFileStruct").
:- pragma foreign_type("Erlang", stream, "").

    % A unique identifier for an I/O stream.
    %
:- type stream_id == int.

:- func get_stream_id(stream) = stream_id.

    % We communicate results from foreign_procs as separate simple arguments
    % so the C/Java/etc code does not depend on how Mercury stores its
    % discriminated union data types. It also avoids memory allocation in
    % inner loops.

:- type result_code
    --->    ok
    ;       eof
    ;       error.

:- pragma foreign_export_enum("C", result_code/0,
    [prefix("ML_RESULT_CODE_"), uppercase]).
:- pragma foreign_export_enum("C#", result_code/0,
    [prefix("ML_RESULT_CODE_"), uppercase]).
:- pragma foreign_export_enum("Java", result_code/0,
    [prefix("ML_RESULT_CODE_"), uppercase]).

    % Reads a character (code point) from specified stream. This may
    % involve converting external character encodings into Mercury's internal
    % character representation and (for text streams) converting OS line
    % indicators, e.g. CR-LF for Windows, to '\n' characters.
    %
:- pred read_char_code(input_stream::in, result_code::out, char::out,
    system_error::out, io::di, io::uo) is det.

    % Reads a byte from specified stream.
    %
:- pred read_byte_val(input_stream::in, result_code::out, int::out,
    system_error::out, io::di, io::uo) is det.

    % call_system_code(Command, Status, Error, !IO):
    %
    % Invokes the operating system shell with the specified Command.
    % Status is valid when Error indicates success.
    %
:- pred call_system_code(string::in, int::out, system_error::out,
    io::di, io::uo) is det.

    % getenv(Var, Value):
    %
    % Gets the value Value associated with the environment variable Var.
    % Fails if the variable was not set.
    %
:- semipure pred getenv(string::in, string::out) is semidet.

    % setenv(NameString, ValueString):
    %
    % Sets the named environment variable to the specified value.
    % Fails if the operation does not work.
    %
:- impure pred setenv(string::in, string::in) is semidet.

%---------------------------------------------------------------------------%
%
% Initialization.
%

init_state(!IO) :-
    init_std_streams(!IO),
    init_current_streams(!IO),
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

:- pred init_current_streams(io::di, io::uo) is det.

init_current_streams(!IO) :-
    %
    % In C grades the "current" streams are thread-local values, so can only be
    % set after the MR_Context has been initialised for the initial thread.
    %
    set_input_stream(stdin_stream, _, !IO),
    set_output_stream(stdout_stream, _, !IO),
    stdin_binary_stream(StdinBinary, !IO),
    stdout_binary_stream(StdoutBinary, !IO),
    set_binary_input_stream(StdinBinary, _, !IO),
    set_binary_output_stream(StdoutBinary, _, !IO).

    % XXX Ditto for C#?
    %
:- pragma foreign_proc("Java",
    init_current_streams(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // Do nothing in the Java grade -- setting the current streams is handled
    // during class initialization.
").

    % Currently no finalization needed...
    % (Perhaps we should close all open Mercury files?
    % That will happen on process exit anyway, so currently we don't bother.)
finalize_state(!IO).

:- pred gc_init(type_desc::in, type_desc::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gc_init(StreamDbType::in, UserGlobalsType::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    /* for Windows DLLs, we need to call GC_INIT() from each DLL */
#ifdef MR_BOEHM_GC
    GC_INIT();
#endif
    MR_add_root(&ML_io_stream_db, (MR_TypeInfo) StreamDbType);
    MR_add_root(&ML_io_user_globals, (MR_TypeInfo) UserGlobalsType);
").

gc_init(_, _, !IO).

:- pred insert_std_stream_names(io::di, io::uo) is det.

insert_std_stream_names(!IO) :-
    stdin_stream(input_stream(Stdin), !IO),
    insert_stream_info(Stdin, stream(0, input, preopen, stdin), !IO),
    stdout_stream(output_stream(Stdout), !IO),
    insert_stream_info(Stdout, stream(1, output, preopen, stdout), !IO),
    stderr_stream(output_stream(Stderr), !IO),
    insert_stream_info(Stderr, stream(1, output, preopen, stderr), !IO).

%---------------------------------------------------------------------------%
%
% Input predicates.
%

% We want to inline these, to allow deforestation.
:- pragma inline(read_char/3).
:- pragma inline(read_char/4).

read_char(Result, !IO) :-
    input_stream(Stream, !IO),
    read_char(Stream, Result, !IO).

read_char(Stream, Result, !IO) :-
    read_char_code(Stream, Result0, Char, Error, !IO),
    (
        Result0 = ok,
        Result = ok(Char)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

:- pragma inline(read_char_unboxed/5).

read_char_unboxed(Stream, Result, Char, !IO) :-
    read_char_code(Stream, Result0, Char, Error, !IO),
    (
        Result0 = ok,
        Result = ok
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

% We want to inline these, to allow deforestation.
:- pragma inline(read_byte/3).
:- pragma inline(read_byte/4).

read_byte(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_byte(Stream, Result, !IO).

read_byte(binary_input_stream(Stream), Result, !IO) :-
    read_byte_val(input_stream(Stream), Result0, Byte, Error, !IO),
    (
        Result0 = ok,
        Result = ok(Byte)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

read_binary_int8(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int8(Stream, Result, !IO).

read_binary_int8(binary_input_stream(Stream), Result, !IO) :-
    read_byte_val(input_stream(Stream), Result0, Int, Error, !IO),
    (
        Result0 = ok,
        Int8 = cast_from_int(Int),
        Result = ok(Int8)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

read_binary_uint8(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint8(Stream, Result, !IO).

read_binary_uint8(binary_input_stream(Stream), Result, !IO) :-
    read_byte_val(input_stream(Stream), Result0, Int, Error, !IO),
    (
        Result0 = ok,
        UInt8 = cast_from_int(Int),
        Result = ok(UInt8)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

read_bitmap(!Bitmap, BytesRead, Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_bitmap(Stream, !Bitmap, BytesRead, Result, !IO).

read_bitmap(StartByte, NumBytes, !Bitmap, BytesRead, Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_bitmap(Stream, StartByte, NumBytes, !Bitmap,
        BytesRead, Result, !IO).

read_bitmap(Stream, !Bitmap, BytesRead, Result, !IO) :-
    ( if NumBytes = !.Bitmap ^ num_bytes then
        io.read_bitmap(Stream, 0, NumBytes, !Bitmap, BytesRead, Result, !IO)
    else
        error("io.read_bitmap: bitmap contains partial final byte")
    ).

read_bitmap(binary_input_stream(Stream), Start, NumBytes, !Bitmap,
        BytesRead, Result, !IO) :-
    ( if
        NumBytes > 0,
        byte_in_range(!.Bitmap, Start),
        byte_in_range(!.Bitmap, Start + NumBytes - 1)
    then
        do_read_bitmap(Stream, Start, NumBytes,
            !Bitmap, 0, BytesRead, Error, !IO),
        ( if is_error(Error, "read failed: ", IOError) then
            Result = error(IOError)
        else
            Result = ok
        )
    else if
        NumBytes = 0,
        byte_in_range(!.Bitmap, Start)
    then
        Result = ok,
        BytesRead = 0
    else
        bitmap.throw_bounds_error(!.Bitmap, "io.read_bitmap",
                Start * bits_per_byte, NumBytes * bits_per_byte)
    ).

:- pred do_read_bitmap(stream::in, byte_index::in, num_bytes::in,
    bitmap::bitmap_di, bitmap::bitmap_uo, num_bytes::in, num_bytes::out,
    system_error::out, io::di, io::uo) is det.

    % Default implementation for Erlang.
do_read_bitmap(Stream, Start, NumBytes, !Bitmap, !BytesRead, Error, !IO) :-
    ( if NumBytes > 0 then
        read_byte_val(input_stream(Stream), Result0, Byte, Error0, !IO),
        (
            Result0 = ok,
            !:Bitmap = !.Bitmap ^ unsafe_byte(Start) := Byte,
            !:BytesRead = !.BytesRead + 1,
            do_read_bitmap(Stream, Start + 1, NumBytes - 1,
                !Bitmap, !BytesRead, Error, !IO)
        ;
            Result0 = eof,
            Error = Error0
        ;
            Result0 = error,
            Error = Error0
        )
    else
        Error = no_error
    ).

:- pragma foreign_proc("C",
    do_read_bitmap(Stream::in, StartByte::in, NumBytes::in,
        Bitmap0::bitmap_di, Bitmap::bitmap_uo, BytesRead0::in, BytesRead::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    size_t nread;

    Bitmap = Bitmap0;
    nread = MR_READ(*Stream, Bitmap->elements + StartByte, NumBytes);
    BytesRead = BytesRead0 + nread;
    if (nread < NumBytes && MR_FERROR(*Stream)) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_read_bitmap(Stream::in, StartByte::in, NumBytes::in,
        Bitmap0::bitmap_di, Bitmap::bitmap_uo, BytesRead0::in, BytesRead::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    io.MR_MercuryFileStruct mf = Stream;

    Bitmap = Bitmap0;
    BytesRead = BytesRead0;

    if (mf.putback != -1) {
        Bitmap.elements[StartByte] = (byte) mf.putback;
        BytesRead++;
        StartByte++;
        NumBytes--;
        mf.putback = -1;
    }

    try {
        BytesRead += mf.stream.Read(Bitmap.elements, StartByte, NumBytes);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_read_bitmap(Stream::in, StartByte::in, NumBytes::in,
        Bitmap0::bitmap_di, Bitmap::bitmap_uo, BytesRead0::in, BytesRead::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    MR_BinaryInputFile mf = (MR_BinaryInputFile) Stream;
    Bitmap = Bitmap0;
    BytesRead = BytesRead0;

    final int nread = mf.read_pushback(Bitmap.elements, StartByte, NumBytes);
    BytesRead += nread;
    StartByte += nread;
    NumBytes -= nread;

    try {
        BytesRead +=
            mf.read_non_pushback(Bitmap.elements, StartByte, NumBytes);
        Error = null;
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

read_binary_file_as_bitmap(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_file_as_bitmap(Stream, Result, !IO).

read_binary_file_as_bitmap(Stream, Result, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    binary_input_stream_file_size(Stream, FileSize, !IO),
    ( if FileSize >= 0 then
        some [!BM] (
            !:BM = bitmap.init(FileSize * bits_per_byte),
            read_bitmap(Stream, 0, FileSize,
                !BM, BytesRead, ReadResult, !IO),
            (
                ReadResult = ok,
                ( if BytesRead = FileSize then
                    Result = ok(!.BM)
                else
                    Result = error(io_error(
                        "io.read_binary_file_as_bitmap: incorrect file size"))
                )
            ;
                ReadResult = error(Msg),
                Result = error(Msg)
            )
        )
    else
        BufferSize = 4000,
        read_binary_file_as_bitmap_2(Stream, BufferSize,
            Res, [], RevBitmaps, !IO),
        (
            Res = ok,
            Result = ok(bitmap.append_list(reverse(RevBitmaps)))
        ;
            Res = error(Msg),
            Result = error(Msg)
        )
    ).

:- pred read_binary_file_as_bitmap_2(io.binary_input_stream::in,
    num_bytes::in, io.res::out, list(bitmap)::in, list(bitmap)::out,
    io::di, io::uo) is det.

read_binary_file_as_bitmap_2(Stream, BufferSize, Res, !BMs, !IO) :-
    some [!BM] (
        !:BM = bitmap.init(BufferSize * bits_per_byte),
        read_bitmap(Stream, 0, BufferSize, !BM, NumBytesRead, ReadRes, !IO),
        (
            ReadRes = ok,
            ( if NumBytesRead < BufferSize then
                !:BM = bitmap.shrink_without_copying(!.BM,
                    NumBytesRead * bits_per_byte),
                !:BMs = [!.BM | !.BMs],
                Res = ok
            else
                !:BMs = [!.BM | !.BMs],

                % Double the buffer size each time.
                read_binary_file_as_bitmap_2(Stream, BufferSize * 2,
                    Res, !BMs, !IO)
            )
        ;
            ReadRes = error(Err),
            Res = error(Err)
        )
    ).

%---------------------------------------------------------------------------%

read_word(Result, !IO) :-
    input_stream(Stream, !IO),
    read_word(Stream, Result, !IO).

read_word(Stream, Result, !IO) :-
    io.ignore_whitespace(Stream, WSResult, !IO),
    (
        WSResult = error(Error),
        Result = error(Error)
    ;
        WSResult = eof,
        Result = eof
    ;
        WSResult = ok,
        read_word_2(Stream, Result, !IO)
    ).

:- pred read_word_2(io.input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

read_word_2(Stream, Result, !IO) :-
    read_char(Stream, CharResult, !IO),
    (
        CharResult = error(Error),
        Result = error(Error)
    ;
        CharResult = eof,
        Result = eof
    ;
        CharResult = ok(Char),
        ( if char.is_whitespace(Char) then
            putback_char(Stream, Char, !IO),
            Result = ok([])
        else
            read_word_2(Stream, Result0, !IO),
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

read_line(Result, !IO) :-
    input_stream(Stream, !IO),
    read_line(Stream, Result, !IO).

read_line(Stream, Result, !IO) :-
    read_line_2(Stream, Result0, Chars, Error, !IO),
    (
        Result0 = ok,
        Result = ok(Chars)
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

:- pred read_line_2(input_stream::in, result_code::out, list(char)::out,
    system_error::out, io::di, io::uo) is det.

read_line_2(Stream, Result, Chars, Error, !IO) :-
    read_char_code(Stream, Result0, Char, Error0, !IO),
    (
        Result0 = ok,
        ( if Char = '\n' then
            Result = ok,
            Chars = [Char],
            Error = Error0
        else
            read_line_2(Stream, Result, CharsTail, Error, !IO),
            Chars = [Char | CharsTail] % lcmc
        )
    ;
        ( Result0 = eof
        ; Result0 = error
        ),
        Result = Result0,
        Chars = [],
        Error = Error0
    ).

read_line_as_string(Result, !IO) :-
    input_stream(Stream, !IO),
    read_line_as_string(Stream, Result, !IO).

read_line_as_string(input_stream(Stream), Result, !IO) :-
    read_line_as_string_2(Stream, yes, Res, String, Error, !IO),
    (
        Res = ok,
        Result = ok(String)
    ;
        Res = eof,
        Result = eof
    ;
        Res = null_char,
        Result = error(io_error("null character in input"))
    ;
        Res = error,
        make_err_msg(Error, "read failed: ", Msg),
        Result = error(io_error(Msg))
    ).

:- type read_line_as_string_result
    --->    ok
    ;       eof
    ;       null_char
    ;       error.

:- pragma foreign_export_enum("C", read_line_as_string_result/0,
    [prefix("ML_READ_LINE_AS_STRING_"), uppercase]).
:- pragma foreign_export_enum("Java", read_line_as_string_result/0,
    [prefix("ML_READ_LINE_AS_STRING_"), uppercase]).

:- pred read_line_as_string_2(io.stream::in, bool::in,
    read_line_as_string_result::out, string::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_line_as_string_2(Stream::in, _FirstCall::in, Res::out,
        RetString::out, Error::out, _IO0::di, _IO::uo),
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

    Res = ML_READ_LINE_AS_STRING_OK;
    Error = 0;
    for (i = 0; char_code != '\\n'; ) {
        char_code = mercury_get_byte(Stream);
        if (char_code == EOF) {
            if (i == 0) {
                if (MR_FERROR(*Stream)) {
                    Res = ML_READ_LINE_AS_STRING_ERROR;
                    Error = errno;
                } else {
                    Res = ML_READ_LINE_AS_STRING_EOF;
                }
            }
            break;
        }
        if (char_code == 0) {
            Res = ML_READ_LINE_AS_STRING_NULL_CHAR;
            break;
        }
        read_buffer[i++] = (char) char_code;
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
    if (Res == ML_READ_LINE_AS_STRING_OK) {
        MR_Word ret_string_word;
        MR_offset_incr_hp_atomic_msg(ret_string_word,
            0, ML_IO_BYTES_TO_WORDS((i + 1) * sizeof(char)),
            MR_ALLOC_ID, ""string.string/0"");
        RetString = (MR_String) ret_string_word;
        MR_memcpy(RetString, read_buffer, i * sizeof(char));
        RetString[i] = '\\0';
    } else {
        RetString = MR_make_string_const("""");
    }
    if (read_buffer != initial_read_buffer) {
        MR_free(read_buffer);
    }
").

:- pragma foreign_proc("Java",
    read_line_as_string_2(Stream::in, _FirstCall::in, Res::out,
        RetString::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate],
"
    try {
        RetString = ((io.MR_TextInputFile) Stream).read_line();
        if (RetString != null) {
            Res = ML_READ_LINE_AS_STRING_OK;
        } else {
            Res = ML_READ_LINE_AS_STRING_EOF;
        }
        Error = null;
    } catch (java.io.IOException e) {
        Res = ML_READ_LINE_AS_STRING_ERROR;
        RetString = """";
        Error = e;
    }
").

read_line_as_string_2(Stream, FirstCall, Res, String, Error, !IO) :-
    % XXX This is terribly inefficient, a better approach would be to
    % use a buffer like what is done for io.read_file_as_string.
    read_char_code(input_stream(Stream), ReadChar, Char, Error0, !IO),
    (
        ReadChar = ok,
        ( if Char = '\n' then
            Res = ok,
            String = "\n",
            Error = Error0
        else if char.to_int(Char, 0) then
            Res = null_char,
            String = "",
            Error = Error0
        else
            read_line_as_string_2(Stream, no, Res, String0, Error, !IO),
            string.first_char(String, Char, String0)
        )
    ;
        ReadChar = eof,
        (
            FirstCall = yes,
            Res = eof
        ;
            FirstCall = no,
            Res = ok
        ),
        String = "",
        Error = Error0
    ;
        ReadChar = error,
        Res = error,
        String = "",
        Error = Error0
    ).

read_file(Result, !IO) :-
    input_stream(Stream, !IO),
    read_file(Stream, Result, !IO).

read_file(Stream, Result, !IO) :-
    read_file_2(Stream, [], Result, !IO).

:- pred read_file_2(input_stream::in, list(char)::in,
    maybe_partial_res(list(char))::out, io::di, io::uo) is det.

read_file_2(Stream, Chars0, Result, !IO) :-
    read_char(Stream, Result0, !IO),
    (
        Result0 = eof,
        Result = ok(list.reverse(Chars0))
    ;
        Result0 = error(Err),
        Result = error(list.reverse(Chars0), Err)
    ;
        Result0 = ok(Char),
        read_file_2(Stream, [Char | Chars0], Result, !IO)
    ).

%---------------------------------------------------------------------------%

read_file_as_string(Result, !IO) :-
    input_stream(Stream, !IO),
    read_file_as_string(Stream, Result, !IO).

read_file_as_string(input_stream(Stream), Result, !IO) :-
    read_file_as_string_2(Stream, String, Error, NullCharError, !IO),
    ( if is_error(Error, "read failed: ", IOError) then
        Result = error(String, IOError)
    else
        (
            NullCharError = yes,
            Result = error("", io_error("null character in input"))
        ;
            NullCharError = no,
            Result = ok(String)
        )
    ).

:- pred read_file_as_string_2(stream::in, string::out, system_error::out,
    bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    read_file_as_string_2(Stream::in, String::out, Error::out,
        NullCharError::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    StringBuilder sb = new StringBuilder();
    try {
        ((io.MR_TextInputFile) Stream).read_file(sb);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
    String = sb.toString();
    NullCharError = bool.NO;
").

:- pragma foreign_proc("Erlang",
    read_file_as_string_2(Stream::in, String::out, Error::out,
        NullCharError::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case mercury__io:mercury_read_string_to_eof(Stream) of
        {ok, String} ->
            Error = ok;
        {error, String, Reason} ->
            Error = {error, Reason}
    end,
    NullCharError = {no}
").

read_file_as_string_2(Stream, String, Error, NullCharError, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    input_stream_file_size(input_stream(Stream), FileSize, !IO),
    ( if FileSize >= 0 then
        BufferSize0 = FileSize + 1
    else
        BufferSize0 = 4000
    ),
    alloc_buffer(BufferSize0, Buffer0),

    % Read the file into the buffer (resizing it as we go if necessary),
    % convert the buffer into a string, and see if anything went wrong.
    Pos0 = 0,
    read_file_as_string_loop(input_stream(Stream), Buffer0, Buffer, Pos0, Pos,
        BufferSize0, BufferSize, Error, !IO),
    require(Pos < BufferSize, "io.read_file_as_string: overflow"),
    ( if buffer_to_string(Buffer, Pos, StringPrime) then
        String = StringPrime,
        NullCharError = no
    else
        String = "",
        NullCharError = yes
    ).

:- pred read_file_as_string_loop(input_stream::in, buffer::buffer_di,
    buffer::buffer_uo, int::in, int::out, int::in, int::out, system_error::out,
    io::di, io::uo) is det.

read_file_as_string_loop(Stream, !Buffer, !Pos, !Size, Error, !IO) :-
    Size0 = !.Size,
    Stream = input_stream(RealStream),
    read_into_buffer(RealStream, !Buffer, !Pos, Size0, Error0, !IO),
    ( if !.Pos < Size0 then
        % Buffer not full: end-of-file or error.
        Error = Error0
    else if !.Pos = Size0 then
        % Full buffer.
        !:Size = Size0 * 2,
        resize_buffer(Size0, !.Size, !Buffer),
        read_file_as_string_loop(Stream, !Buffer, !Pos, !Size, Error, !IO)
    else
        error("io.read_file_as_string: buffer overflow")
    ).

%---------------------------------------------------------------------------%

input_stream_foldl(Pred, T0, Res, !IO) :-
    input_stream(Stream, !IO),
    input_stream_foldl(Stream, Pred, T0, Res, !IO).

input_stream_foldl(Stream, Pred, T0, Res, !IO) :-
    read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, T0, T1),
        input_stream_foldl(Stream, Pred, T1, Res, !IO)
    ;
        CharResult = eof,
        Res = ok(T0)
    ;
        CharResult = error(Error),
        Res = error(T0, Error)
    ).

input_stream_foldl_io(Pred, Res, !IO) :-
    input_stream(Stream, !IO),
    input_stream_foldl_io(Stream, Pred, Res, !IO).

input_stream_foldl_io(Stream, Pred, Res, !IO) :-
    read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, !IO),
        input_stream_foldl_io(Stream, Pred, Res, !IO)
    ;
        CharResult = eof,
        Res = ok
    ;
        CharResult = error(Error),
        Res = error(Error)
    ).

input_stream_foldl2_io(Pred, T0, Res, !IO) :-
    input_stream(Stream, !IO),
    input_stream_foldl2_io(Stream, Pred, T0, Res, !IO).

input_stream_foldl2_io(Stream, Pred, T0, Res, !IO) :-
    read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, T0, T1, !IO),
        input_stream_foldl2_io(Stream, Pred, T1, Res, !IO)
    ;
        CharResult = eof,
        Res = ok(T0)
    ;
        CharResult = error(Error),
        Res = error(T0, Error)
    ).

input_stream_foldl2_io_maybe_stop(Pred, T0, Res, !IO) :-
    input_stream(Stream, !IO),
    input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO).

input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO) :-
    read_char(Stream, CharResult, !IO),
    (
        CharResult = ok(Char),
        Pred(Char, Continue, T0, T1, !IO),
        (
            Continue = no,
            Res = ok(T1)
        ;
            Continue = yes,
            input_stream_foldl2_io_maybe_stop(Stream, Pred, T1, Res, !IO)
        )
    ;
        CharResult = eof,
        Res = ok(T0)
    ;
        CharResult = error(Error),
        Res = error(T0, Error)
    ).

%---------------------------------------------------------------------------%

:- pred input_stream_file_size(io.input_stream::in, int::out,
    io::di, io::uo) is det.

input_stream_file_size(input_stream(Stream), Size, !IO) :-
    stream_file_size(Stream, Size, !IO).

:- pred binary_input_stream_file_size(io.binary_input_stream::in, int::out,
    io::di, io::uo) is det.

binary_input_stream_file_size(binary_input_stream(Stream), Size, !IO) :-
    stream_file_size(Stream, Size, !IO).

    % stream_file_size(Stream, Size):
    % If Stream is a regular file, then Size is its size (in bytes),
    % otherwise Size is -1.
    %
:- pred stream_file_size(stream::in, int::out, io::di, io::uo) is det.

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
    stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
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
    stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    if (Stream.stream.CanSeek) {
        Size = (int) Stream.stream.Length;
    } else {
        Size = -1;
    }
}").

:- pragma foreign_proc("Java",
    stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Size = ((MR_BinaryFile) Stream).size();
    } catch (java.io.IOException e) {
        Size = -1;
    }
").

:- pragma foreign_proc("Erlang",
    stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Size = mercury__io:mercury_get_file_size(Stream)
").

%---------------------------------------------------------------------------%

file_modification_time(File, Result, !IO) :-
    file_modification_time_2(File, Time, Error, !IO),
    ( if is_error(Error, "can't get file modification time: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(Time)
    ).

:- pred file_modification_time_2(string::in, time_t::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
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
        /* XXX avoid ML_construct_time_t by returning time_t_rep? */
        Time = ML_construct_time_t(s.st_mtime);
        Error = 0;
    } else {
        Error = errno;
        Time = 0;
    }
#else
    Error = ENOSYS;
    Time = 0;
#endif
").

:- pragma foreign_proc("C#",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (System.IO.File.Exists(FileName)) {
            System.DateTime t = System.IO.File.GetLastWriteTime(FileName);
            Time = time.ML_construct_time_t(t);
            Error = null;
        } else {
            Error = new System.IO.FileNotFoundException();
            Time = null;
        }
    } catch (System.Exception e) {
        Error = e;
        Time = null;
    }
").

:- pragma foreign_proc("Java",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        long modtime = (new java.io.File(FileName)).lastModified();
        if (modtime == 0) {
            Error = new java.io.FileNotFoundException(
                ""File not found or I/O error"");
            Time = null;
        } else {
            Time = time.ML_construct_time_t(new java.util.Date(modtime));
            Error = null;
        }
    } catch (java.lang.Exception e) {
        Error = e;
        Time = null;
    }
").

:- pragma foreign_proc("Erlang",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    FileNameStr = binary_to_list(FileName),
    case filelib:last_modified(FileNameStr) of
        {YMD, HMS} ->
            % time_t in Erlang is in UTC.
            Time = {time_t, erlang:localtime_to_universaltime({YMD, HMS})},
            Error = ok;
        0 ->
            Error = {error, enoent},
            Time = {time_t, erlang:localtime()}
    end
").

%---------------------------------------------------------------------------%

:- pragma foreign_export_enum("C", file_type/0,
    [prefix("ML_FILE_TYPE_"), uppercase]).
:- pragma foreign_export_enum("C#", file_type/0,
    [prefix("ML_FILE_TYPE_"), uppercase]).
:- pragma foreign_export_enum("Java", file_type/0,
    [prefix("ML_FILE_TYPE_"), uppercase]).

file_type(FollowSymLinks, FileName, Result, !IO) :-
    (
        FollowSymLinks = yes,
        FollowSymLinksInt = 1
    ;
        FollowSymLinks = no,
        FollowSymLinksInt = 0
    ),
    file_type_2(FollowSymLinksInt, FileName, FileType, Error,  !IO),
    ( if is_error(Error, "can't find file type: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(FileType)
    ).

:- pred file_type_2(int::in, string::in, file_type::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    file_type_2(FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
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
        /* Do we still need the non-POSIX S_IFMT style? */
        if
            #if defined(S_ISREG)
                (S_ISREG(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFREG)
                ((s.st_mode & S_IFMT) == S_IFREG)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_REGULAR_FILE;
        }
        else if
            #if defined(S_ISDIR)
                (S_ISDIR(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFDIR)
                ((s.st_mode & S_IFMT) == S_IFDIR)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_DIRECTORY;
        }
        else if
            #if defined(S_ISBLK)
                (S_ISBLK(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFBLK)
                ((s.st_mode & S_IFMT) == S_IFBLK)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_BLOCK_DEVICE;
        }
        else if
            #if defined(S_ISCHR)
                (S_ISCHR(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFCHR)
                ((s.st_mode & S_IFMT) == S_IFCHR)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_CHARACTER_DEVICE;
        }
        else if
            #if defined(S_ISFIFO)
                (S_ISFIFO(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFIFO)
                ((s.st_mode & S_IFMT) == S_IFIFO)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_NAMED_PIPE;
        }
        else if
            #if defined(S_ISLNK)
                (S_ISLNK(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFLNK)
                ((s.st_mode & S_IFMT) == S_IFLNK)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_SYMBOLIC_LINK;
        }
        else if
            #if defined(S_ISSOCK)
                (S_ISSOCK(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFSOCK)
                ((s.st_mode & S_IFMT) == S_IFSOCK)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_SOCKET;
        } else {

        #ifdef S_TYPEISMQ
            if (S_TYPEISMQ(&s)) {
                FileType = ML_FILE_TYPE_MESSAGE_QUEUE;
            } else
        #endif

        #ifdef S_TYPEISSEM
            if (S_TYPEISSEM(&s)) {
                FileType = ML_FILE_TYPE_SEMAPHORE;
            } else
        #endif

        #ifdef S_TYPEISSHM
            if (S_TYPEISSHM(&s)) {
                FileType = ML_FILE_TYPE_SHARED_MEMORY;
            } else
        #endif

            {
                FileType = ML_FILE_TYPE_UNKNOWN;
            }
        }
        Error = 0;
    } else {
        FileType = ML_FILE_TYPE_UNKNOWN;
        Error = errno;
    }
#else
    FileType = ML_FILE_TYPE_UNKNOWN;
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    file_type_2(_FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        System.IO.FileAttributes attrs =
            System.IO.File.GetAttributes(FileName);
        if ((attrs & System.IO.FileAttributes.Directory) ==
            System.IO.FileAttributes.Directory)
        {
            FileType = io.ML_FILE_TYPE_DIRECTORY;
        }
        else if ((attrs & System.IO.FileAttributes.Device) ==
            System.IO.FileAttributes.Device)
        {
            // XXX It may be a block device, but .NET doesn't
            // distinguish between character and block devices.
            FileType = io.ML_FILE_TYPE_CHARACTER_DEVICE;
        }
        else
        {
            FileType = io.ML_FILE_TYPE_REGULAR_FILE;
        }
        Error = null;
    } catch (System.Exception e) {
        FileType = ML_FILE_TYPE_UNKNOWN;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    file_type_2(_FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // The Java implementation can distinguish between regular files and
    // directories, and for everything else it just returns unknown.

    FileType = io.ML_FILE_TYPE_UNKNOWN;
    Error = null;

    try {
        java.io.File file = new java.io.File(FileName);
        if (file.isFile()) {
            FileType = io.ML_FILE_TYPE_REGULAR_FILE;
        } else if (file.isDirectory()) {
            FileType = io.ML_FILE_TYPE_DIRECTORY;
        } else if (file.exists()) {
            FileType = io.ML_FILE_TYPE_UNKNOWN;
        } else {
            Error = new java.io.FileNotFoundException(
                ""File not found or I/O error"");
        }
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

:- pragma foreign_decl("Erlang", local, "
-include_lib(""kernel/include/file.hrl"").
").

:- pragma foreign_proc("Erlang",
    file_type_2(FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
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
                    FileType = {character_device};
                directory ->
                    FileType = {directory};
                regular ->
                    FileType = {regular_file};
                symlink ->
                    FileType = {symbolic_link};
                other ->
                    FileType = {unknown}
            end,
            Error = ok;
        {error, Reason} ->
            FileType = {unknown},
            Error = {error, Reason}
    end
").

%---------------------------------------------------------------------------%

check_file_accessibility(FileName, AccessTypes, Result, !IO) :-
    ( if have_dotnet then
        check_file_accessibility_dotnet(FileName, AccessTypes, Result, !IO)
    else
        CheckRead = pred_to_bool(contains(AccessTypes, read)),
        CheckWrite = pred_to_bool(contains(AccessTypes, write)),
        CheckExecute = pred_to_bool(contains(AccessTypes, execute)),
        check_file_accessibility_2(FileName, CheckRead, CheckWrite,
            CheckExecute, Error, !IO),
        ( if is_error(Error, "file not accessible: ", IOError) then
            Result = error(IOError)
        else
            Result = ok
        )
    ).

:- pred check_file_accessibility_2(string::in, bool::in, bool::in, bool::in,
    system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    check_file_accessibility_2(FileName::in, CheckRead::in,
        CheckWrite::in, CheckExecute::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_HAVE_ACCESS)
  #ifdef F_OK
    const int MODE_EXISTS = F_OK;
  #else
    const int MODE_EXISTS = 0;
  #endif
  #ifdef X_OK
    const int MODE_EXECUTE = X_OK;
  #else
    const int MODE_EXECUTE = 1;
  #endif
  #ifdef W_OK
    const int MODE_WRITE = W_OK;
  #else
    const int MODE_WRITE = 2;
  #endif
  #ifdef R_OK
    const int MODE_READ = R_OK;
  #else
    const int MODE_READ = 4;
  #endif

    int mode = MODE_EXISTS;
    int access_result;

  #if !defined(MR_WIN32) || defined(MR_CYGWIN)
    /*
    ** Earlier versions of MSVCRT ignored flags it doesn't support,
    ** later versions return an error (e.g. on Vista).
    */
    if (CheckExecute) {
        mode |= MODE_EXECUTE;
    }
  #endif
    if (CheckWrite) {
        mode |= MODE_WRITE;
    }
    if (CheckRead) {
        mode |= MODE_READ;
    }

  #ifdef MR_WIN32
    access_result = _waccess(ML_utf8_to_wide(FileName), mode);
  #else
    access_result = access(FileName, mode);
  #endif

    if (access_result == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
#else /* !MR_HAVE_ACCESS */
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("Java",
    check_file_accessibility_2(FileName::in, CheckRead::in, CheckWrite::in,
        CheckExecute::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    java.io.File file = new java.io.File(FileName);
    try {
        boolean ok = true;

        if (CheckRead == bool.YES) {
            ok = file.canRead();
        }

        if (ok && CheckWrite == bool.YES) {
            ok = file.canWrite();
        }

        if (ok && CheckExecute == bool.YES) {
            ok = file.canExecute();
        }

        if (ok) {
            Error = null;
        } else {
            Error = new java.io.FileNotFoundException(""Permission denied"");
        }
    }
    catch (java.lang.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    check_file_accessibility_2(FileName::in, CheckRead::in, CheckWrite::in,
        _CheckExecute::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate],
"
    FileNameStr = binary_to_list(FileName),
    case file:read_file_info(FileNameStr) of
        {ok, FileInfo} ->
            Access = FileInfo#file_info.access,
            case CheckRead of
                {yes} ->
                    Ok0 = lists:member(Access, [read, read_write]);
                {no} ->
                    Ok0 = true
            end,
            case Ok0 of
                true ->
                    case CheckWrite of
                        {yes} ->
                            Ok = lists:member(Access, [write, read_write]);
                        {no} ->
                            Ok = true
                    end;
                false ->
                    Ok = Ok0
            end,
            % XXX test execute access somehow
            case Ok of
                true ->
                    Error = ok;
                false ->
                    Error = {error, eacces}
            end
    ;
        {error, Reason} ->
            Error = {error, Reason}
    end
").

    % XXX why not write this as check_file_accessibility_2?
    %
:- pred check_file_accessibility_dotnet(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.

check_file_accessibility_dotnet(FileName, AccessTypes, Result, !IO) :-
    % The .NET CLI doesn't provide an equivalent of access(), so we have to
    % try to open the file to see if it is accessible.

    CheckRead0 = pred_to_bool(contains(AccessTypes, read)),
    CheckWrite = pred_to_bool(contains(AccessTypes, write)),
    CheckExec = pred_to_bool(contains(AccessTypes, execute)),
    % We need to be able to read a file to execute it.
    CheckRead = bool.or(CheckRead0, CheckExec),

    file_type(yes, FileName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(FileType),
        ( if FileType = directory then
            check_directory_accessibility_dotnet(FileName,
                CheckRead, CheckWrite, Error, !IO),
            ( if is_error(Error, "file not accessible: ", IOError) then
                Result = error(IOError)
            else
                Result = ok
            )
        else
            (
                CheckRead = yes,
                open_input(FileName, InputRes, !IO),
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
            ( if
                CheckReadRes = ok,
                CheckWrite = yes
            then
                open_append(FileName, OutputRes, !IO),
                (
                    OutputRes = ok(OutputStream),
                    io.close_output(OutputStream, !IO),
                    CheckWriteRes = ok
                ;
                    OutputRes = error(OutputError),
                    CheckWriteRes = error(OutputError)
                )
            else
                CheckWriteRes = CheckReadRes
            ),
            ( if
                CheckWriteRes = ok,
                % Unix programs need to check whether the execute bit is set
                % for the directory, but we can't actually execute the
                % directory.
                CheckExec = yes
            then
                have_dotnet_exec_permission(Error, !IO),
                ( if is_error(Error, "file not accessible: ", IOError) then
                    Result = error(IOError)
                else
                    Result = ok
                )
            else
                Result = CheckWriteRes
            )
        )
    ;
        FileTypeRes = error(FileTypeError),
        Result = error(FileTypeError)
    ).

:- pred have_dotnet_exec_permission(system_error::out, io::di, io::uo) is det.

have_dotnet_exec_permission(Error, !IO) :-
    % Avoid determinism warnings.
    ( if semidet_succeed then
        error("io.have_dotnet_exec_permission invoked " ++
            "for non-.NET CLI backend")
    else
        % Never reached.
        Error = no_error
    ).

:- pragma foreign_proc("C#",
    have_dotnet_exec_permission(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        // We need unrestricted permissions to execute unmanaged code.
        (new System.Security.Permissions.SecurityPermission(
            System.Security.Permissions.SecurityPermissionFlag.
            AllFlags)).Demand();
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pred check_directory_accessibility_dotnet(string::in, bool::in, bool::in,
    system_error::out, io::di, io::uo) is det.

check_directory_accessibility_dotnet(_, _, _, Error, !IO) :-
    % Avoid determinism warnings.
    ( if semidet_succeed then
        error("io.check_directory_accessibility_dotnet called " ++
            "for non-.NET CLI backend")
    else
        % Never reached.
        Error = no_error
    ).

:- pragma foreign_proc("C#",
    check_directory_accessibility_dotnet(FileName::in, CheckRead::in,
        CheckWrite::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Error = null;
        if (CheckRead == mr_bool.YES) {
            // XXX This is less efficient than I would like.
            // Unfortunately the .NET CLI has no function
            // corresponding to access() or opendir().
            System.IO.Directory.GetFileSystemEntries(FileName);
        }
        if (CheckWrite == mr_bool.YES) {
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
            // io.make_temp, but currently the C# backend version of that
            // ignores the directory passed to it.
            System.IO.FileAttributes attrs =
                System.IO.File.GetAttributes(FileName);
            if ((attrs & System.IO.FileAttributes.ReadOnly) ==
                System.IO.FileAttributes.ReadOnly)
            {
                Error = new System.Exception(""file is read-only"");
            }
        }
    } catch (System.Exception e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

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
    ( if Result0 < 0 then
        Result = (<)
    else if Result0 = 0 then
        Result = (=)
    else
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
    throw new RuntimeException(""File IDs are not supported by Java."");
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

file_id(FileName, Result, !IO) :-
    io.file_id_2(FileName, FileId, Error, !IO),
    ( if is_error(Error, "cannot get file id: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(FileId)
    ).

:- pred file_id_2(string::in, file_id::out, system_error::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    file_id_2(FileName::in, FileId::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /* Win32 returns junk in the st_ino field of `struct stat'. */
#if defined(MR_HAVE_STAT) && !defined(MR_WIN32)
    struct stat s;
    int stat_result = stat(FileName, &s);

    if (stat_result == 0) {
        FileId.device = s.st_dev;
        FileId.inode = s.st_ino;
        Error = 0;
    } else {
        Error = errno;
    }
#else
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    file_id_2(_FileName::in, _FileId::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Error = new System.NotSupportedException(
        ""io.file_id not supported on this platform"");
").

:- pragma foreign_proc("Java",
    file_id_2(_FileName::in, _FileId::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Error = new java.lang.UnsupportedOperationException(
        ""io.file_id not supported on this platform"");
").

:- pragma foreign_proc("Erlang",
    file_id_2(FileName::in, FileId::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    FileNameStr = binary_to_list(FileName),
    case file:read_file_info(FileNameStr) of
        {ok, FileInfo} ->
            MajorDevice = FileInfo#file_info.major_device,
            Inode = FileInfo#file_info.inode,
            FileId = {MajorDevice, Inode},
            Error = ok;
        {error, Reason} ->
            FileId = null,
            Error = {error, Reason}
    end
").

%---------------------------------------------------------------------------%

% A `buffer' is an array of chars.
% For C backends, it is a C array of C chars.
% For other backends, it is a Mercury array of Mercury chars.

:- type buffer.
:- pragma foreign_type(c, buffer, "char *", [can_pass_as_mercury_type]).

    % XXX It would be better to use a char_array type rather than array(char).
    % This is because on the Java and IL backends indexing into an array whose
    % element type is known statically requires less overhead.
:- type buffer ---> buffer(array(char)).

    % XXX Extend the workaround for no `ui' modes in array.m.
:- inst uniq_buffer == bound(buffer(uniq_array)).
:- mode buffer_di == di(uniq_buffer).
:- mode buffer_uo == out(uniq_buffer).

:- pred alloc_buffer(int::in, buffer::buffer_uo) is det.

:- pragma foreign_proc("C",
    alloc_buffer(Size::in, Buffer::buffer_uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"{
    MR_Word buf;
    MR_offset_incr_hp_atomic_msg(buf, 0,
        (Size * sizeof(char) + sizeof(MR_Word) - 1) / sizeof(MR_Word),
        MR_ALLOC_ID, ""io.buffer/0"");
    Buffer = (char *) buf;
}").

alloc_buffer(Size, buffer(Array)) :-
    char.det_from_int(0, NullChar),
    array.init(Size, NullChar, Array).

:- pred resize_buffer(int::in, int::in,
    buffer::buffer_di, buffer::buffer_uo) is det.

:- pragma foreign_proc("C",
    resize_buffer(OldSize::in, NewSize::in,
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

resize_buffer(_OldSize, NewSize, buffer(Array0), buffer(Array)) :-
    char.det_from_int(0, Char),
    array.resize(NewSize, Char, Array0, Array).

:- pred buffer_to_string(buffer::buffer_di, int::in, string::uo) is semidet.

:- pragma foreign_proc("C",
    buffer_to_string(Buffer::buffer_di, Len::in, Str::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"{
    Str = Buffer;
    Str[Len] = '\\0';

    /* Check that the string doesn't contain null characters. */
    if (strlen(Str) != Len) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").

buffer_to_string(buffer(Array), Len, String) :-
    array.fetch_items(Array, min(Array), min(Array) + Len - 1, List),
    string.semidet_from_char_list(List, String).

:- pred read_into_buffer(stream::in, buffer::buffer_di, buffer::buffer_uo,
    int::in, int::out, int::in, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_into_buffer(Stream::in, Buffer0::buffer_di, Buffer::buffer_uo,
        Pos0::in, Pos::out, Size::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    size_t bytes_to_read;
    size_t bytes_read;

    MR_CHECK_EXPR_TYPE(Buffer0, char *);
    MR_CHECK_EXPR_TYPE(Buffer, char *);

    bytes_to_read = Size - Pos0;
    bytes_read = MR_READ(*Stream, Buffer0 + Pos0, bytes_to_read);

    Buffer = Buffer0;
    Pos = Pos0 + bytes_read;

    if (bytes_read < bytes_to_read && MR_FERROR(*Stream)) {
        Error = errno;
    } else {
        Error = 0;
    }
").

read_into_buffer(Stream, buffer(Array0), buffer(Array), Pos0, Pos, Size,
        Error, !IO) :-
    read_into_array(input_stream(Stream), Array0, Array, Pos0, Pos, Size,
        Error, !IO).

:- pred read_into_array(input_stream::in,
    array(char)::array_di, array(char)::array_uo, int::in, int::out,
    int::in, system_error::out, io::di, io::uo) is det.

read_into_array(Stream, !Array, !Pos, Size, Error, !IO) :-
    ( if !.Pos >= Size then
        Error = no_error
    else
        read_char_code(Stream, Result, Char, Error0, !IO),
        (
            Result = ok,
            array.set(!.Pos, Char, !Array),
            !:Pos = !.Pos + 1,
            read_into_array(Stream, !Array, !Pos, Size, Error, !IO)
        ;
            Result = eof,
            Error = Error0
        ;
            Result = error,
            Error = Error0
        )
    ).

%---------------------------------------------------------------------------%

read_binary_file(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_file(Stream, Result, !IO).

read_binary_file(Stream, Result, !IO) :-
    read_binary_file_2(Stream, [], Result, !IO).

:- pred read_binary_file_2(binary_input_stream::in, list(int)::in,
    io.result(list(int))::out, io::di, io::uo) is det.

read_binary_file_2(Stream, Bytes0, Result, !IO) :-
    read_byte(Stream, Result0, !IO),
    (
        Result0 = eof,
        list.reverse(Bytes0, Bytes),
        Result = ok(Bytes)
    ;
        Result0 = error(Err),
        Result = error(Err)
    ;
        Result0 = ok(Byte),
        read_binary_file_2(Stream, [Byte | Bytes0], Result, !IO)
    ).

%---------------------------------------------------------------------------%

binary_input_stream_foldl(Pred, T0, Res, !IO) :-
    binary_input_stream(Stream, !IO),
    binary_input_stream_foldl(Stream, Pred, T0, Res, !IO).

binary_input_stream_foldl(Stream, Pred, T0, Res, !IO) :-
    read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, T0, T1),
        binary_input_stream_foldl(Stream, Pred, T1, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok(T0)
    ;
        ByteResult = error(Error),
        Res = error(T0, Error)
    ).

binary_input_stream_foldl_io(Pred, Res, !IO) :-
    binary_input_stream(Stream, !IO),
    binary_input_stream_foldl_io(Stream, Pred, Res, !IO).

binary_input_stream_foldl_io(Stream, Pred, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        binary_input_stream_foldl_io_plain(Stream, Pred, Res, !IO)
    ;
        ShouldReduce = yes,
        io.binary_input_stream_foldl_io_chunk(Stream, Pred, Res, !IO)
    ).

:- pred binary_input_stream_foldl_io_plain(binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode binary_input_stream_foldl_io_plain(in,
    (pred(in, di, uo) is det), out, di, uo) is det.
:- mode binary_input_stream_foldl_io_plain(in,
    (pred(in, di, uo) is cc_multi), out, di, uo) is cc_multi.

binary_input_stream_foldl_io_plain(Stream, Pred, Res, !IO) :-
    read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, !IO),
        binary_input_stream_foldl_io_plain(Stream, Pred, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok
    ;
        ByteResult = error(Error),
        Res = error(Error)
    ).

:- pred binary_input_stream_foldl_io_chunk(binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode binary_input_stream_foldl_io_chunk(in,
    (pred(in, di, uo) is det), out, di, uo) is det.
:- mode binary_input_stream_foldl_io_chunk(in,
    (pred(in, di, uo) is cc_multi), out, di, uo) is cc_multi.

binary_input_stream_foldl_io_chunk(Stream, Pred, Res, !IO) :-
    binary_input_stream_foldl_io_inner(chunk_size, Stream, Pred,
        InnerRes, !IO),
    (
        InnerRes = ok,
        Res = ok
    ;
        InnerRes = error(Error),
        Res = error(Error)
    ;
        InnerRes = more,
        binary_input_stream_foldl_io_chunk(Stream, Pred, Res, !IO)
    ).

:- pred binary_input_stream_foldl_io_inner(int, binary_input_stream,
    pred(int, io, io), chunk_inner_res0, io, io).
:- mode binary_input_stream_foldl_io_inner(in, in,
    (pred(in, di, uo) is det), out, di, uo) is det.
:- mode binary_input_stream_foldl_io_inner(in, in,
    (pred(in, di, uo) is cc_multi), out, di, uo) is cc_multi.

binary_input_stream_foldl_io_inner(Left, Stream, Pred, Res, !IO) :-
    ( if Left > 0 then
        read_byte(Stream, ByteResult, !IO),
        (
            ByteResult = ok(Byte),
            Pred(Byte, !IO),
            binary_input_stream_foldl_io_inner(Left - 1,
                Stream, Pred, Res, !IO)
        ;
            ByteResult = eof,
            Res = ok
        ;
            ByteResult = error(Error),
            Res = error(Error)
        )
    else
        Res = more
    ).

binary_input_stream_foldl2_io(Pred, T0, Res, !IO) :-
    binary_input_stream(Stream, !IO),
    binary_input_stream_foldl2_io(Stream, Pred, T0, Res, !IO).

binary_input_stream_foldl2_io(Stream, Pred, T0, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        binary_input_stream_foldl2_io_plain(Stream, Pred, T0, Res, !IO)
    ;
        ShouldReduce = yes,
        binary_input_stream_foldl2_io_chunk(Stream, Pred, T0, Res, !IO)
    ).

:- pred binary_input_stream_foldl2_io_plain(binary_input_stream,
    pred(int, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io_plain(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_plain(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_plain(Stream, Pred, T0, Res, !IO) :-
    read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, T0, T1, !IO),
        binary_input_stream_foldl2_io_plain(Stream, Pred, T1, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok(T0)
    ;
        ByteResult = error(Error),
        Res = error(T0, Error)
    ).

:- pred binary_input_stream_foldl2_io_chunk(binary_input_stream,
    pred(int, T, T, io, io), T, io.maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io_chunk(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_chunk(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_chunk(Stream, Pred, T0, Res, !IO) :-
    binary_input_stream_foldl2_io_inner(chunk_size, Stream, Pred, T0,
        InnerRes, !IO),
    (
        InnerRes = ok(T),
        Res = ok(T)
    ;
        InnerRes = error(T, Error),
        Res = error(T, Error)
    ;
        InnerRes = more(T1),
        binary_input_stream_foldl2_io_chunk(Stream, Pred, T1, Res, !IO)
    ).

:- pred binary_input_stream_foldl2_io_inner(int, binary_input_stream,
    pred(int, T, T, io, io), T, chunk_inner_res(T), io, io).
:- mode binary_input_stream_foldl2_io_inner(in, in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_inner(in, in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_inner(Left, Stream, Pred, T0, Res, !IO) :-
    ( if Left > 0 then
        read_byte(Stream, ByteResult, !IO),
        (
            ByteResult = ok(Byte),
            Pred(Byte, T0, T1, !IO),
            binary_input_stream_foldl2_io_inner(Left - 1,
                Stream, Pred, T1, Res, !IO)
        ;
            ByteResult = eof,
            Res = ok(T0)
        ;
            ByteResult = error(Error),
            Res = error(T0, Error)
        )
    else
        Res = more(T0)
    ).

binary_input_stream_foldl2_io_maybe_stop(Pred, T0, Res, !IO) :-
    binary_input_stream(Stream, !IO),
    binary_input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO).

binary_input_stream_foldl2_io_maybe_stop(Stream, Pred, T0, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        binary_input_stream_foldl2_io_maybe_stop_plain(Stream,
            Pred, T0, Res, !IO)
    ;
        ShouldReduce = yes,
        binary_input_stream_foldl2_io_maybe_stop_chunk(Stream,
            Pred, T0, Res, !IO)
    ).

:- pred binary_input_stream_foldl2_io_maybe_stop_plain(
    binary_input_stream, pred(int, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io_maybe_stop_plain(
    in, (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_maybe_stop_plain(
    in, (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_maybe_stop_plain(Stream, Pred, T0, Res,
        !IO) :-
    read_byte(Stream, ByteResult, !IO),
    (
        ByteResult = ok(Byte),
        Pred(Byte, Continue, T0, T1, !IO),
        (
            Continue = no,
            Res = ok(T1)
        ;
            Continue = yes,
            binary_input_stream_foldl2_io_maybe_stop_plain(
                Stream, Pred, T1, Res, !IO)
        )
    ;
        ByteResult = eof,
        Res = ok(T0)
    ;
        ByteResult = error(Error),
        Res = error(T0, Error)
    ).

:- pred binary_input_stream_foldl2_io_maybe_stop_chunk(
    binary_input_stream, pred(int, bool, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io_maybe_stop_chunk(
    in, (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_maybe_stop_chunk(
    in, (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_maybe_stop_chunk(Stream, Pred, T0, Res,
        !IO) :-
    binary_input_stream_foldl2_io_maybe_stop_inner(chunk_size,
        Stream, Pred, T0, InnerRes, !IO),
    (
        InnerRes = ok(T),
        Res = ok(T)
    ;
        InnerRes = error(T, Error),
        Res = error(T, Error)
    ;
        InnerRes = more(T1),
        binary_input_stream_foldl2_io_maybe_stop_chunk(Stream,
            Pred, T1, Res, !IO)
    ).

:- pred binary_input_stream_foldl2_io_maybe_stop_inner(int,
    binary_input_stream, pred(int, bool, T, T, io, io),
    T, chunk_inner_res(T), io, io).
:- mode binary_input_stream_foldl2_io_maybe_stop_inner(in,
    in, (pred(in, out, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io_maybe_stop_inner(in,
    in, (pred(in, out, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

binary_input_stream_foldl2_io_maybe_stop_inner(Left, Stream, Pred, T0, Res,
        !IO) :-
    ( if Left > 0 then
        read_byte(Stream, ByteResult, !IO),
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
    else
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
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
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

%---------------------------------------------------------------------------%

putback_char(Char, !IO) :-
    input_stream(Stream, !IO),
    putback_char(Stream, Char, !IO).

putback_byte(Char, !IO) :-
    binary_input_stream(Stream, !IO),
    putback_byte(Stream, Char, !IO).

read(Result, !IO) :-
    term_io.read_term(ReadResult, !IO),
    get_line_number(LineNumber, !IO),
    process_read_term(ReadResult, LineNumber, Result).

read_from_string(FileName, String, Len, Result, !Posn) :-
    parser.read_term_from_substring(FileName, String, Len, !Posn, ReadResult),
    !.Posn = posn(LineNumber, _, _),
    process_read_term(ReadResult, LineNumber, Result).

:- pred process_read_term(read_term::in, int::in, io.read_result(T)::out)
    is det.

process_read_term(ReadResult, LineNumber, Result) :-
    (
        ReadResult = term(_VarSet, Term),
        ( if term_to_type(Term, Type) then
            Result = ok(Type)
        else
            ( if term.is_ground(Term) then
                Result = error(
                    "io.read: the term read did not have the right type",
                    LineNumber)
            else
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

read(Stream, Result, !IO) :-
    % The term parser does not accept an explicit stream argument (yet)
    % so we must change the current input stream.
    with_input_stream(Stream, read, Result, !IO).

ignore_whitespace(Result, !IO) :-
    input_stream(Stream, !IO),
    ignore_whitespace(Stream, Result, !IO).

ignore_whitespace(Stream, Result, !IO) :-
    read_char(Stream, CharResult, !IO),
    (
        CharResult = error(Error),
        Result = error(Error)
    ;
        CharResult = eof,
        Result = eof
    ;
        CharResult = ok(Char),
        ( if char.is_whitespace(Char) then
            ignore_whitespace(Stream, Result, !IO)
        else
            putback_char(Stream, Char, !IO),
            Result = ok
        )
    ).

%---------------------------------------------------------------------------%
%
% Output predicates.
%

nl(!IO) :-
    write_char('\n', !IO).

nl(Stream, !IO) :-
    write_char(Stream, '\n', !IO).

write_strings(Strings, !IO) :-
    output_stream(Stream, !IO),
    write_strings(Stream, Strings, !IO).

write_strings(_Stream, [], !IO).
write_strings(Stream, [S | Ss], !IO) :-
    write_string(Stream, S, !IO),
    write_strings(Stream, Ss, !IO).

format(FormatString, Arguments, !IO) :-
    output_stream(Stream, !IO),
    io.format(Stream, FormatString, Arguments, !IO).

format(Stream, FormatString, Arguments, !IO) :-
    string.format(FormatString, Arguments, String),
    write_string(Stream, String, !IO).

write_many(Poly_list, !IO) :-
    output_stream(Stream, !IO),
    write_many(Stream, Poly_list, !IO).

write_many(_Stream, [], !IO).
write_many(Stream, [c(C) | Rest], !IO) :-
    write_char(Stream, C, !IO),
    write_many(Stream, Rest, !IO).
write_many(Stream, [i(I) | Rest], !IO) :-
    write_int(Stream, I, !IO),
    write_many(Stream, Rest, !IO).
write_many(Stream, [s(S) | Rest], !IO) :-
    write_string(Stream, S, !IO),
    write_many(Stream, Rest, !IO).
write_many(Stream, [f(F) | Rest], !IO) :-
    write_float(Stream, F, !IO),
    write_many(Stream, Rest, !IO).

%---------------------------------------------------------------------------%
%
% Various different versions of io.print.
%

print(Term, !IO) :-
    output_stream(Stream, !IO),
    stream.string_writer.print(Stream, canonicalize, Term, !IO).

print(Stream, Term, !IO) :-
    stream.string_writer.print(Stream, canonicalize, Term, !IO).

print(Stream, NonCanon, Term, !IO) :-
    stream.string_writer.print(Stream, NonCanon, Term, !IO).

print_cc(Term, !IO) :-
    output_stream(Stream, !IO),
    stream.string_writer.print_cc(Stream, Term, !IO).

:- pred io.print_to_stream(io.stream::in, T::in, io::di, io::uo) is det.

    % XXX Only use this for debugging. Strictly speaking, io.print may throw an
    % exception which is not allowed across the C interface.
    %
:- pragma foreign_export("C", io.print_to_stream(in, in, di, uo),
    "ML_io_print_to_stream").

print_to_stream(Stream, Term, !IO) :-
    io.print(output_stream(Stream), canonicalize, Term, !IO).

print_line(Term, !IO) :-
    io.output_stream(Stream, !IO),
    print_line(Stream, Term, !IO).

print_line(Stream, Term, !IO) :-
    io.print(Stream, Term, !IO),
    io.nl(Stream, !IO).

print_line(Stream, NonCanon, Term, !IO) :-
    io.print(Stream, NonCanon, Term, !IO),
    io.nl(Stream, !IO).

print_line_cc(Term, !IO) :-
    io.print_cc(Term, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
%
% Various different versions of io.write.
%

write(X, !IO) :-
    output_stream(Stream, !IO),
    stream.string_writer.write(Stream, canonicalize, X, !IO).

write(Stream, X, !IO) :-
    stream.string_writer.write(Stream, canonicalize, X, !IO).

write(Stream, NonCanon, X, !IO) :-
    stream.string_writer.write(Stream, NonCanon, X, !IO).

write_cc(X, !IO) :-
    output_stream(Stream, !IO),
    io.write_cc(Stream, X, !IO).

write_cc(Stream, X, !IO) :-
    stream.string_writer.write(Stream, include_details_cc, X, !IO).

write_line(X, !IO) :-
    io.write(X, !IO),
    io.nl(!IO).

write_line(Stream, X, !IO) :-
    io.write(Stream, X, !IO),
    io.nl(Stream, !IO).

write_line(Stream, NonCanon, X, !IO) :-
    io.write(Stream, NonCanon, X, !IO),
    io.nl(Stream, !IO).

write_line_cc(X, !IO) :-
    io.write_cc(X, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

write_list([], _Separator, _OutputPred, !IO).
write_list([Head | Tail], Separator, OutputPred, !IO) :-
    OutputPred(Head, !IO),
    (
        Tail = []
    ;
        Tail = [TailHead | TailTail],
        write_list_lag(TailHead, TailTail, Separator, OutputPred, !IO)
    ).

:- pred write_list_lag(T, list(T), string, pred(T, io, io), io, io).
:- mode write_list_lag(in, in, in, pred(in, di, uo) is det, di, uo) is det.
:- mode write_list_lag(in, in, in, pred(in, di, uo) is cc_multi, di, uo)
    is cc_multi.

write_list_lag(Head, Tail, Separator, OutputPred, !IO) :-
    write_string(Separator, !IO),
    OutputPred(Head, !IO),
    (
        Tail = []
    ;
        Tail = [TailHead | TailTail],
        write_list_lag(TailHead, TailTail, Separator, OutputPred, !IO)
    ).

write_list(Stream, List, Separator, OutputPred, !IO) :-
    % OutputPred expects the current output stream to be Stream.
    with_output_stream(Stream,
        write_list(List, Separator, OutputPred), !IO).

%---------------------------------------------------------------------------%

write_array(Array, Separator, OutputPred, !IO) :-
    array.bounds(Array, Lo, Hi),
    ( if Lo =< Hi then
        array.unsafe_lookup(Array, Lo, E),
        OutputPred(E, !IO),
        do_write_array(Array, Separator, OutputPred, Lo + 1, Hi, !IO)
    else
        true
    ).

:- pred do_write_array(array(T), string, pred(T, io, io), int, int, io, io).
:- mode do_write_array(in, in, pred(in, di, uo) is det, in, in, di, uo)
    is det.
:- mode do_write_array(in, in, pred(in, di, uo) is cc_multi, in, in, di, uo)
    is cc_multi.

do_write_array(Array, Separator, OutputPred, I, Hi, !IO) :-
    ( if I =< Hi then
        write_string(Separator, !IO),
        array.unsafe_lookup(Array, I, E),
        OutputPred(E, !IO),
        do_write_array(Array, Separator, OutputPred, I + 1, Hi, !IO)
    else
        true
    ).

write_array(Stream, Array, Separator, OutputPred, !IO) :-
    % OutputPred expects the current output stream to be Stream.
    with_output_stream(Stream,
        write_array(Array, Separator, OutputPred), !IO).

%---------------------------------------------------------------------------%

write_binary(Term, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary(Stream, Term, !IO).

write_binary(binary_output_stream(Stream), Term, !IO) :-
    % A quick-and-dirty implementation... not very space-efficient
    % (not really binary!)
    % XXX This will not work for the Java back-end. See the comment at the
    % top of the MR_MercuryFileStruct class definition.
    io.write(output_stream(Stream), Term, !IO),
    io.write_string(output_stream(Stream), ".\n", !IO).

read_binary(Result, !IO) :-
    % A quick-and-dirty implementation... not very space-efficient
    % (not really binary!)
    % XXX This will not work for the Java back-end. See the comment at the
    % top of the MR_MercuryFileStruct class definition.
    binary_input_stream(binary_input_stream(Stream), !IO),
    with_input_stream(input_stream(Stream),
        read_binary_from_current_input_stream, Result, !IO).

read_binary(binary_input_stream(Stream), Result, !IO) :-
    % We would prefer not to change the current input stream but the
    % quick-and-dirty implementation of read_binary uses io.read,
    % and io.read internally reads from the current text input stream.
    % XXX This will not work for the Java back-end. See the comment at the
    % top of the MR_MercuryFileStruct class definition.
    with_input_stream(input_stream(Stream),
        read_binary_from_current_input_stream, Result, !IO).

:- pred read_binary_from_current_input_stream(io.result(T)::out,
    io::di, io::uo) is det.

read_binary_from_current_input_stream(Result, !IO) :-
    read(ReadResult, !IO),
    (
        ReadResult = ok(T),
        % We've read the newline and the trailing full stop.
        % Now skip the newline after the full stop.
        read_char(NewLineRes, !IO),
        (
            NewLineRes = error(Error),
            Result = error(Error)
        ;
            NewLineRes = ok(NewLineChar),
            ( if NewLineChar = '\n' then
                Result = ok(T)
            else
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

%---------------------------------------------------------------------------%
%
% Stream predicates.
%

open_input(FileName, Result, !IO) :-
    do_open_text(FileName, "r", OpenCount, NewStream, Error, !IO),
    ( if is_error(Error, "can't open input file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(input_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, input, text, file(FileName)), !IO)
    ).

open_output(FileName, Result, !IO) :-
    do_open_text(FileName, "w", OpenCount, NewStream, Error, !IO),
    ( if is_error(Error, "can't open output file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, output, text, file(FileName)), !IO)
    ).

open_append(FileName, Result, !IO) :-
    do_open_text(FileName, "a", OpenCount, NewStream, Error, !IO),
    ( if is_error(Error, "can't append to file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, append, text, file(FileName)), !IO)
    ).

open_binary_input(FileName, Result, !IO) :-
    do_open_binary(FileName, "rb", OpenCount, NewStream, Error, !IO),
    ( if is_error(Error, "can't open input file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(binary_input_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, input, binary, file(FileName)), !IO)
    ).

open_binary_output(FileName, Result, !IO) :-
    do_open_binary(FileName, "wb", OpenCount, NewStream, Error, !IO),
    ( if is_error(Error, "can't open output file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(binary_output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, output, binary, file(FileName)), !IO)
    ).

open_binary_append(FileName, Result, !IO) :-
    do_open_binary(FileName, "ab", OpenCount, NewStream, Error, !IO),
    ( if is_error(Error, "can't append to file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(binary_output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, append, binary, file(FileName)), !IO)
    ).

%---------------------------------------------------------------------------%
%
% Declarative versions of Prolog's see/1 and seen/0.
%

see(File, Result, !IO) :-
    io.open_input(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_input_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

seen(!IO) :-
    io.stdin_stream(Stdin, !IO),
    io.set_input_stream(Stdin, OldStream, !IO),
    io.close_input(OldStream, !IO).

% Plus binary IO versions.

see_binary(File, Result, !IO) :-
    io.open_binary_input(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_binary_input_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

seen_binary(!IO) :-
    io.stdin_binary_stream(Stdin, !IO),
    io.set_binary_input_stream(Stdin, OldStream, !IO),
    io.close_binary_input(OldStream, !IO).

%---------------------------------------------------------------------------%
%
% Declarative versions of Prolog's tell/1 and told/0.
%

told(!IO) :-
    io.stdout_stream(Stdout, !IO),
    io.set_output_stream(Stdout, OldStream, !IO),
    io.close_output(OldStream, !IO).

tell(File, Result, !IO) :-
    io.open_output(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_output_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

told_binary(!IO) :-
    io.stdout_binary_stream(Stdout, !IO),
    io.set_binary_output_stream(Stdout, OldStream, !IO),
    io.close_binary_output(OldStream, !IO).

tell_binary(File, Result, !IO) :-
    io.open_binary_output(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_binary_output_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Stream name predicates.
%

input_stream_name(Name, !IO) :-
    input_stream(input_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

input_stream_name(input_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

output_stream_name(Name, !IO) :-
    output_stream(output_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

output_stream_name(output_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

binary_input_stream_name(Name, !IO) :-
    binary_input_stream(binary_input_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

binary_input_stream_name(binary_input_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

binary_output_stream_name(Name, !IO) :-
    binary_output_stream(binary_output_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

binary_output_stream_name(binary_output_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

:- pred stream_name(stream::in, string::out, io::di, io::uo) is det.

stream_name(Stream, Name, !IO) :-
    stream_info(Stream, MaybeInfo, !IO),
    (
        MaybeInfo = yes(Info),
        Info = stream(_, _, _, Source),
        Name = source_name(Source)
    ;
        MaybeInfo = no,
        Name = "<stream name unavailable>"
    ).

:- pred stream_info(io.stream::in, maybe(stream_info)::out,
    io::di, io::uo) is det.

stream_info(Stream, MaybeInfo, !IO) :-
    lock_stream_db(!IO),
    get_stream_db(StreamDb, !IO),
    unlock_stream_db(!IO),
    ( if map.search(StreamDb, get_stream_id(Stream), Info) then
        MaybeInfo = yes(Info)
    else
        MaybeInfo = no
    ).

input_stream_info(StreamDb, input_stream(Stream)) =
    maybe_stream_info(StreamDb, Stream).

output_stream_info(StreamDb, output_stream(Stream)) =
    maybe_stream_info(StreamDb, Stream).

binary_input_stream_info(StreamDb, binary_input_stream(Stream)) =
    maybe_stream_info(StreamDb, Stream).

binary_output_stream_info(StreamDb, binary_output_stream(Stream)) =
    maybe_stream_info(StreamDb, Stream).

:- func maybe_stream_info(io.stream_db, io.stream) = maybe_stream_info.

maybe_stream_info(StreamDb, Stream) = Info :-
    ( if map.search(StreamDb, get_stream_id(Stream), Info0) then
        % Info0 and Info have different types.
        Info0 = stream(Id, Mode, Content, Source),
        Info  = stream(Id, Mode, Content, Source)
    else
        Info  = unknown_stream
    ).

get_io_stream_info(StreamDB, Stream) = StreamInfo :-
    ( if dynamic_cast(Stream, input_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, output_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, binary_input_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, binary_output_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, IOStream0) then
        IOStream = IOStream0
    else
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
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    MR_LOCK(&ML_io_stream_db_lock, ""io.get_stream_db/1"");
    StreamDb = ML_io_stream_db;
    MR_UNLOCK(&ML_io_stream_db_lock, ""io.get_stream_db/1"");
").

    % Caller must hold the stream_db lock.
    %
:- pragma foreign_proc("C",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness],
"
    StreamDb = ML_io_stream_db;
").

    % Caller must hold the stream_db lock.
    %
:- pred set_stream_db(io.stream_db::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ML_io_stream_db = StreamDb;
").

:- pred lock_stream_db(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    io.lock_stream_db(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        no_sharing],
"
    MR_LOCK(&ML_io_stream_db_lock, ""io.lock_stream_db/2"");
").

lock_stream_db(!IO).

:- pred unlock_stream_db(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unlock_stream_db(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        no_sharing],
"
    MR_UNLOCK(&ML_io_stream_db_lock, ""io.unlock_stream_db/2"");
").

unlock_stream_db(!IO).

:- pragma foreign_proc("C#",
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("C#",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("C#",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_stream_db = StreamDb;
").

:- pragma foreign_proc("Java",
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("Java",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = io.ML_io_stream_db;
").

:- pragma foreign_proc("Java",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_stream_db = StreamDb;
").

% XXX the following Erlang implementation doesn't work with multiple threads

:- pragma foreign_proc("Erlang",
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = get('ML_io_stream_db')
").

:- pragma foreign_proc("Erlang",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = get('ML_io_stream_db')
").

:- pragma foreign_proc("Erlang",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    put('ML_io_stream_db', StreamDb)
").

%---------------------------------------------------------------------------%

:- pred insert_stream_info(stream::in, stream_info::in, io::di, io::uo)
    is det.

insert_stream_info(Stream, Name, !IO) :-
    lock_stream_db(!IO),
    get_stream_db(StreamDb0, !IO),
    map.set(get_stream_id(Stream), Name, StreamDb0, StreamDb),
    set_stream_db(StreamDb, !IO),
    unlock_stream_db(!IO).

:- pred maybe_delete_stream_info(io.stream::in, io::di, io::uo) is det.

maybe_delete_stream_info(Stream, !IO) :-
    may_delete_stream_info(MayDeleteStreamInfo, !IO),
    ( if MayDeleteStreamInfo = 0 then
        true
    else
        lock_stream_db(!IO),
        get_stream_db(StreamDb0, !IO),
        map.delete(get_stream_id(Stream), StreamDb0, StreamDb),
        set_stream_db(StreamDb, !IO),
        unlock_stream_db(!IO)
    ).

    % Return an integer that is nonzero if and only if we should delete
    % the information we have about stream when that stream is closed.
    % The debugger may need this information in order to display the stream id
    % in a user-friendly manner even after the stream is closed (e.g. after
    % performing a retry after the close), so if debugging is enabled, we
    % hang on to the stream info until the end of the execution. This is a
    % space leak, but one that is acceptable in a program being debugged.
    %
:- pred may_delete_stream_info(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    may_delete_stream_info(MayDelete::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    MayDelete = !MR_debug_ever_enabled;
").

may_delete_stream_info(1, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Global state predicates.
%

get_globals(Globals, !IO) :-
    lock_globals(!IO),
    unsafe_get_globals(Globals, !IO),
    unlock_globals(!IO).

set_globals(Globals, !IO) :-
    lock_globals(!IO),
    unsafe_set_globals(Globals, !IO),
    unlock_globals(!IO).

:- pragma promise_pure(update_globals/3).

update_globals(UpdatePred, !IO) :-
    lock_globals(!IO),
    unsafe_get_globals(Globals0, !IO),
    promise_equivalent_solutions [!:IO] (
        Update = (pred(G::out) is det :-
            UpdatePred(Globals0, G)
        ),
        try(Update, UpdateResult),
        (
            UpdateResult = succeeded(Globals),
            unsafe_set_globals(Globals, !IO),
            unlock_globals(!IO)
        ;
            % If the update operation threw an exception
            % then release the lock and rethrow the exception.
            UpdateResult = exception(_),
            impure io.unlock_globals,
            rethrow(UpdateResult)
        )
    ).

:- pred lock_globals(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    lock_globals(_IO0::di, _IO::uo),
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

:- pred unlock_globals(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    unlock_globals(_IO0::di, _IO::uo),
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

:- impure pred unlock_globals is det.
:- pragma foreign_proc("C",
    unlock_globals,
    [will_not_call_mercury, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    #ifdef MR_THREAD_SAFE
        MR_UNLOCK(&ML_io_user_globals_lock, \"io.unlock_globals/2\");
    #endif
").
    % For the non-C backends.
    %
unlock_globals :-
    impure impure_true.

    % NOTE: io.unsafe_{get, set}_globals/3 are marked as `thread_safe' so that
    % calling them to does not acquire the global lock. Since calls to these
    % predicates should be surrounded by calls to io.{lock, unlock}_globals/2
    % this is safe.
    %
:- pred unsafe_get_globals(univ::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    unsafe_get_globals(Globals::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    Globals = ML_io_user_globals;
").

:- pred unsafe_set_globals(univ::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    unsafe_set_globals(Globals::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /* XXX need to globalize the memory */
    ML_io_user_globals = Globals;
").

:- pragma foreign_proc("C#",
    unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = io.ML_io_user_globals;
").

:- pragma foreign_proc("C#",
    unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_user_globals = Globals;
").

:- pragma foreign_proc("Java",
    unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Globals = io.ML_io_user_globals;
").

:- pragma foreign_proc("Java",
    unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    io.ML_io_user_globals = Globals;
").

% XXX the following Erlang implementation doesn't work with multiple threads

:- pragma foreign_proc("Erlang",
    unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = get('ML_io_user_globals')
").

:- pragma foreign_proc("Erlang",
    unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    put('ML_io_user_globals', Globals)
").

%---------------------------------------------------------------------------%

progname_base(DefaultName, PrognameBase, !IO) :-
    progname(DefaultName, Progname, !IO),
    PrognameBase = dir.det_basename(Progname).

:- pragma foreign_proc("C",
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
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
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure],
"
    Id = Stream.id;
").

:- pragma foreign_proc("Java",
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    Id = Stream.id;
").

:- pragma foreign_proc("Erlang",
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    {'ML_stream', Id, _IoDevice} = Stream
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Environment interface predicates
%

:- pragma promise_pure(get_environment_var/4).

get_environment_var(Var, OptValue, !IO) :-
    ( if semipure getenv(Var, Value) then
        OptValue0 = yes(Value)
    else
        OptValue0 = no
    ),
    OptValue = OptValue0.

:- pragma promise_pure(set_environment_var/5).

set_environment_var(Var, Value, Res, !IO) :-
    ( if have_set_environment_var then
        ( if impure setenv(Var, Value) then
            Res = ok
        else
            string.format("Could not set environment variable `%s'",
                [s(Var)], Message),
            Res = error(io_error(Message))
        )
    else
        Message = "Cannot set environment variables on this platform",
        Res = error(io_error(Message))
    ).

set_environment_var(Var, Value, IO0, IO) :-
    io.set_environment_var(Var, Value, Res, IO0, IO1),
    (
        Res = ok,
        IO = IO1
    ;
        Res = error(ErrorCode),
        error(io.error_message(ErrorCode))
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Statistics reporting predicates.
%

report_stats(!IO) :-
    report_stats("standard", !IO).

:- pragma promise_pure(io.report_stats/3).

report_stats(Selector, !IO) :-
    ( if Selector = "standard" then
        impure report_stats
    else if Selector = "full_memory_stats" then
        impure report_full_memory_stats
    else if Selector = "tabling" then
        impure table_builtin.table_report_statistics
    else
        string.format("io.report_stats: selector `%s' not understood",
            [s(Selector)], Message),
        error(Message)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Miscellaneous predicates.
%

call_system(Command, Result, !IO) :-
    call_system_return_signal(Command, Result0, !IO),
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

call_system_return_signal(Command, Result, !IO) :-
    call_system_code(Command, Status, Error, !IO),
    ( if is_error(Error, "error invoking system command: ", IOError) then
        Result = error(IOError)
    else
        Result = decode_system_command_exit_code(Status)
    ).

%---------------------------------------------------------------------------%

:- type io.error
    --->    io_error(string).       % This is subject to change.
    % Note that we use `io_error' rather than `io.error' because io.print,
    % which may be called to print out the uncaught exception if there is no
    % exception handler, does not print out the module name.

make_io_error(Error) = io_error(Error).

error_message(Error) = Msg :-
    io.error_message(Error, Msg).

error_message(io_error(Error), Error).

%---------------------------------------------------------------------------%

    % XXX design flaw with regard to unique modes and
    % io.get_op_table

get_op_table(ops.init_mercury_op_table, !IO).

set_op_table(_OpTable, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% The remaining predicates are implemented using the C interface.

:- pragma foreign_decl("C", "

#include ""mercury_init.h""
#include ""mercury_wrapper.h""
#include ""mercury_type_info.h""
#include ""mercury_library_types.h""
#include ""mercury_file.h""
#include ""mercury_heap.h""
#include ""mercury_misc.h""
#include ""mercury_runtime_util.h""

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
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
int             mercury_get_byte(MercuryFilePtr mf);
int             mercury_close(MercuryFilePtr mf);
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
        // the `reader' field may be null. Any code which accesses that
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
    ** Binary stdin and stdout are a special case. They are opened via
    ** FileInput/OutputStreams and seeking is controlled through use of
    ** FileChannels (requiring Java versions >= 1.4).
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
        ** charset decoding. Returns -1 at end of file.
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
        ** charset decoding. Returns null at end of file.
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
            ** to make room at the front of the buffer. If the buffer is full
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

        public void flush()
            throws java.io.IOException
        {
            output.flush();
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

        // channel is used for positioning the stream. Read/write operations
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
                c = pushback.pop() & 0xff; // make unsigned
            }
            return c;
        }

        public void ungetc(byte b) {
            pushback.push(b);
        }

        public int read_pushback(byte[] b, int start, int len)
        {
            final int end = start + len;
            int cur = start;
            while (cur < end && !pushback.empty()) {
                b[cur] = pushback.pop();
                cur++;
            }
            return cur - start;
        }

        public int read_non_pushback(byte[] b, int start, int len)
            throws java.io.IOException
        {
            int n = binary_input.read(b, start, len);
            if (n < 0) {
                return 0;
            }
            return n;
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

        // Obsolete.
        public void write(java.lang.String s)
            throws java.io.IOException
        {
            for (int i = 0; i < s.length(); i++) {
                // lower 8 bits of each
                put((byte) s.charAt(i));
            }
        }

        public void write(byte[] bs, int start, int length)
            throws java.io.IOException
        {
            binary_output.write(bs, start, length);
        }

        public void flush()
            throws java.io.IOException
        {
            binary_output.flush();
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

#if defined(MR_HAVE_FDOPEN) && (defined(MR_HAVE_FILENO) || defined(fileno)) \
        && defined(MR_HAVE_DUP)
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
        **       in *write* mode. Setting binary stdin to stdin in such
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
    ** to binary mode. I guess we just have to punt...
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
    // of /dev/null. This could perhaps be considered a problem. But if so,
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
").

:- pragma foreign_decl("Erlang", local, "

    % These need to be exported because code in foreign_procs may be inlined
    % into other modules. Hence, calls to these functions must be module
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
    mercury_write_binary/2,
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
    % Note that we send back acknowledgements for all messages. This is to
    % ensure that two operations from the same process are done in order.
    %
mercury_start_file_server(ParentPid, FileName, Mode) ->
    % XXX This is wrong for binary streams.
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
    % XXX This is wrong for binary streams.
    io:setopts(IoDevice, [binary, {encoding, utf8}]),
    mercury_file_server(IoDevice, 1, []).

mercury_file_server(IoDevice, LineNr0, PutBack0) ->
    receive
        {From, close} ->
            Result = file:close(IoDevice),
            From ! {self(), close_ack, Result}
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
        {From, write_binary, Binary} ->
            From ! {self(), write_binary_ack},
            % XXX return error code
            io:put_chars(IoDevice, Binary),
            mercury_file_server(IoDevice, LineNr0, PutBack0)
    ;
        {From, sync} ->
            % XXX file:sync seems to hang if run on a pid, e.g. standard I/O
            if
                is_pid(IoDevice) ->
                    Result = ok;
                true ->
                    Result = file:sync(IoDevice)
            end,
            From ! {self(), sync_ack, Result},
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
            From ! {self(), get_file_size_ack, Size},
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

    % Returns {ok, Stream} | {error, Reason}
    %
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

    % Returns ok | {error, Reason}
    %
mercury_close_stream(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), close},
    receive
        {Pid, close_ack, Result} ->
            Result
    end.

    % Returns <integer> | eof | {error, Reason}
    %
mercury_getc(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), read_char},
    receive
        {Pid, read_char_ack, Result} ->
            Result
    end.

    % Returns {ok, Binary} | {error, Partial, Reason}
    %
mercury_read_string_to_eof(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), read_string_to_eof},
    receive
        {Pid, read_string_to_eof_ack, Result} ->
            Result
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

mercury_write_binary(Stream, Binary) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), write_binary, Binary},
    receive
        {Pid, write_binary_ack} ->
            void
    end.

    % Returns ok | {error, Reason}
    %
mercury_sync(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), sync},
    receive
        {Pid, sync_ack, Result} ->
            Result
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

    % Returns {ok, NewPosition} | {error, Reason}
    %
mercury_get_pos(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), get_pos},
    receive
        {Pid, get_pos_ack, Result} ->
            Result
    end.

    % Returns {ok, NewPosition} | {error, Reason}
    %
mercury_set_pos(Stream, Loc) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), set_pos, Loc},
    receive
        {Pid, set_pos_ack, Result} ->
            Result
    end.

    % Returns Size or -1
    %
mercury_get_file_size(Stream) ->
    {'ML_stream', _Id, Pid} = Stream,
    Pid ! {self(), get_file_size},
    receive
        {Pid, get_file_size_ack, Result} ->
            Result
    end.

mercury_set_current_text_input(Stream) ->
    put('ML_io_current_text_input', Stream).

mercury_set_current_text_output(Stream) ->
    put('ML_io_current_text_output', Stream).

mercury_set_current_binary_input(Stream) ->
    put('ML_io_current_binary_input', Stream).

mercury_set_current_binary_output(Stream) ->
    put('ML_io_current_binary_output', Stream).

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

    /*
    ** Check if the opened file is actually a directory.
    ** If fileno or fstat are not available then we assume the OS would not
    ** let us open a directory as a file anyway.
    */
#if defined(MR_HAVE_FSTAT) && \
        (defined(MR_HAVE_FILENO) || defined(fileno)) && defined(S_ISDIR)
    {
        struct stat stat_info;
        int         stat_errno;

        if (0 != fstat(fileno(f), &stat_info)) {
            stat_errno = errno;
            fclose(f);
            errno = stat_errno;
            return NULL;
        }
        if (S_ISDIR(stat_info.st_mode)) {
            fclose(f);
            errno = EISDIR;
            return NULL;
        }
    }
#endif

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

    if (openmode == ""r"" || openmode == ""rb"") {
        // Like '<' in Bourne shell.
        // Read a file. The file must exist already.
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
    // simultaneously. XXX Is this a good idea?
    share = System.IO.FileShare.ReadWrite;

    stream = System.IO.File.Open(filename, mode, access, share);

    // We initialize the `reader' and `writer' fields to null;
    // they will be filled in later if they are needed.
    return mercury_file_init(new System.IO.BufferedStream(stream),
        null, null, line_ending);
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
// and possibly converting CR-LF to newline. Returns -1 on EOF, and
// throws an exception on error.

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
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma foreign_code("C", "

#include <errno.h>

#ifdef EBADF
  #define MR_CLOSED_FILE_ERROR  EBADF
#else
  /* ANSI/ISO C guarantees that EDOM will exist */
  #define MR_CLOSED_FILE_ERROR  EDOM
#endif

#ifdef MR_NEW_MERCURYFILE_STRUCT

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

static int
ME_closed_stream_ferror(MR_StreamInfo *info)
{
    return 0;
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
    /* putc     = */    ME_closed_stream_putch,
    /* ferror   = */    ME_closed_stream_ferror
};

#endif /* MR_NEW_MERCURYFILE_STRUCT */

int
mercury_close(MercuryFilePtr mf)
{
    /*
    ** On some systems attempting to close a file stream that has been
    ** previously closed will lead to a segmentation fault. We check
    ** that we have not previously closed the file stream here so we
    ** can give the user some idea about what has happened.
    */
    if (MR_file(*mf) == NULL) {
        errno = MR_CLOSED_FILE_ERROR;
        return EOF;
    }

    if (MR_CLOSE(*mf) < 0) {
        return EOF;
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

    return 0;
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

%---------------------------------------------------------------------------%
%
% Platform checks
%

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

%---------------------------------------------------------------------------%
%
% Errors
%

:- pragma foreign_decl("C", "

#include <string.h>
#include <errno.h>

/*
** ML_make_err_msg(errnum, msg, alloc_id, error_msg):
** Append `msg' and a message for errnum to give `error_msg'.
**
** WARNING: this must only be called when the `hp' register is valid.
** That means it must only be called from procedures declared
** `[will_not_call_mercury, promise_pure]'.
**
** This is defined as a macro rather than a C function
** to avoid worrying about the `hp' register being
** invalidated by the function call.
*/

#define ML_make_err_msg(errnum, msg, alloc_id, error_msg)                   \\
    do {                                                                    \\
        char    errbuf[MR_STRERROR_BUF_SIZE];                               \\
        const char *errno_msg;                                              \\
        size_t  total_len;                                                  \\
                                                                            \\
        errno_msg = MR_strerror(errnum, errbuf, sizeof(errbuf));            \\
        total_len = strlen(msg) + strlen(errno_msg);                        \\
        MR_allocate_aligned_string_msg((error_msg), total_len, (alloc_id)); \\
        strcpy((error_msg), msg);                                           \\
        strcat((error_msg), errno_msg);                                     \\
    } while(0)

/*
** ML_make_win32_err_msg(error, msg, alloc_id, error_msg):
** Append `msg' and the string returned by the Win32 API function
** FormatMessage() for the last error to give `error_msg'.
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

#define ML_make_win32_err_msg(error, msg, alloc_id, error_msg)              \\
    do {                                                                    \\
        size_t total_len;                                                   \\
        LPVOID  err_buf;                                                    \\
        MR_bool free_err_buf = MR_TRUE;                                     \\
                                                                            \\
        if (!FormatMessage(                                                 \\
                FORMAT_MESSAGE_ALLOCATE_BUFFER                              \\
                | FORMAT_MESSAGE_FROM_SYSTEM                                \\
                | FORMAT_MESSAGE_IGNORE_INSERTS,                            \\
                NULL,                                                       \\
                error,                                                      \\
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),                  \\
                (LPTSTR) &err_buf,                                          \\
                0,                                                          \\
                NULL))                                                      \\
        {                                                                   \\
            free_err_buf = MR_FALSE;                                        \\
            err_buf = (LPVOID) ""could not retrieve error message"";        \\
        }                                                                   \\
        total_len = strlen(msg) + strlen((char *)err_buf);                  \\
        MR_allocate_aligned_string_msg((error_msg), total_len, (alloc_id)); \\
        strcpy((error_msg), msg);                                           \\
        strcat((error_msg), (char *)err_buf);                               \\
        if (free_err_buf) {                                                 \\
            LocalFree(err_buf);                                             \\
        }                                                                   \\
    } while(0)

#endif /* !MR_WIN32 */
").

:- func no_error = system_error.

:- pragma foreign_proc("C",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = 0;
").

:- pragma foreign_proc("C#",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = null;
").

:- pragma foreign_proc("Java",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = null;
").

:- pragma foreign_proc("Erlang",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = ok
").

:- pred is_success(system_error::in) is semidet.

:- pragma foreign_proc("C",
    is_success(Error::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* This works for errno and Win32 error values (ERROR_SUCCESS == 0). */
    SUCCESS_INDICATOR = (Error == 0) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    is_success(Error::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Error == null);
").

:- pragma foreign_proc("Java",
    is_success(Error::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Error == null);
").

:- pragma foreign_proc("Erlang",
    is_success(Error::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Error =:= ok)
").

is_error(Error, Prefix, io_error(Message)) :-
    ( if is_success(Error) then
        fail
    else
        make_err_msg(Error, Prefix, Message)
    ).

is_maybe_win32_error(Error, Prefix, io_error(Message)) :-
    ( if is_success(Error) then
        fail
    else
        make_maybe_win32_err_msg(Error, Prefix, Message)
    ).

:- pragma foreign_proc("C",
    make_err_msg(Error::in, Msg0::in, Msg::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    ML_make_err_msg(Error, Msg0, MR_ALLOC_ID, Msg);
").

:- pragma foreign_proc("C#",
    make_err_msg(Error::in, Msg0::in, Msg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Msg = System.String.Concat(Msg0, Error.Message);
").

:- pragma foreign_proc("Java",
    make_err_msg(Error::in, Msg0::in, Msg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Error.getMessage() != null) {
        Msg = Msg0 + Error.getMessage();
    } else {
        Msg = Msg0;
    }
").

:- pragma foreign_proc("Erlang",
    make_err_msg(Error::in, Msg0::in, Msg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    case Error of
        {error, Reason} ->
            Msg = list_to_binary([Msg0, file:format_error(Reason)]);
        _ ->
            Msg = Msg0
    end
").

make_maybe_win32_err_msg(Error, Msg0, Msg) :-
    ( if have_win32 then
        make_win32_err_msg(Error, Msg0, Msg)
    else
        make_err_msg(Error, Msg0, Msg)
    ).

:- pred make_win32_err_msg(system_error::in, string::in, string::out) is det.

make_win32_err_msg(_, _, "") :-
    ( if semidet_succeed then
        error("io.make_win32_err_msg called for non Win32 back-end")
    else
        true
    ).

    % Is FormatMessage thread-safe?
    %
:- pragma foreign_proc("C",
    make_win32_err_msg(Error::in, Msg0::in, Msg::out),
    [will_not_call_mercury, promise_pure, does_not_affect_liveness,
        no_sharing],
"
#ifdef MR_WIN32
    ML_make_win32_err_msg(Error, Msg0, MR_ALLOC_ID, Msg);
#else
    MR_fatal_error(""io.make_win32_err_msg called on non-Windows platform"");
#endif
").

:- pred throw_on_error(system_error::in, string::in, io::di, io::uo) is det.

throw_on_error(Error, Prefix, !IO) :-
    ( if is_error(Error, Prefix, IOError) then
        throw(IOError)
    else
        true
    ).

:- pred throw_on_output_error(system_error::in, io::di, io::uo) is det.

throw_on_output_error(Error, !IO) :-
    throw_on_error(Error, "error writing to output file: ", !IO).

:- pred throw_on_close_error(system_error::in, io::di, io::uo) is det.

throw_on_close_error(Error, !IO) :-
    throw_on_error(Error, "error closing file: ", !IO).

%---------------------------------------------------------------------------%
%
% Input predicates.
%

read_char_code(input_stream(Stream), Result, Char, Error, !IO) :-
    read_char_code_2(Stream, Result, Char, Error, !IO).

:- pred read_char_code_2(stream::in, result_code::out, char::out,
    system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_char_code_2(Stream::in, Result::out, Char::out, Error::out,
        _IO0::di, _IO::uo),
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
        Result = ML_RESULT_CODE_OK;
        Char = uc;
        Error = 0;
    } else if (c == EOF) {
        if (MR_FERROR(*Stream)) {
            Result = ML_RESULT_CODE_ERROR;
            Error = errno;
        } else {
            Result = ML_RESULT_CODE_EOF;
            Error = 0;
        }
        Char = 0;
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
            buf[0] = (char) uc;
            for (i = 1; i < nbytes; i++) {
                c = mercury_get_byte(Stream);
                uc = c;
                if (c == EOF) {
                    /* Illegal byte sequence whether EOF or I/O error. */
                    Result = ML_RESULT_CODE_ERROR;
                    Error = MR_FERROR(*Stream) ? errno : EILSEQ;
                    Char = 0;
                    break;
                }
                buf[i] = uc;
            }
            if (i == nbytes) {
                buf[i] = '\\0';
                c = MR_utf8_get(buf, 0);
                if (c < 0) {
                    Result = ML_RESULT_CODE_ERROR;
                    Error = EILSEQ;
                    Char = 0;
                } else {
                    Result = ML_RESULT_CODE_OK;
                    Char = c;
                    Error = 0;
                }
            }
        } else {
            /* Invalid lead byte. */
            Result = ML_RESULT_CODE_ERROR;
            Error = EILSEQ;
            Char = 0;
        }
    }
").

read_byte_val(input_stream(Stream), Result, ByteVal, Error, !IO) :-
    read_byte_val_2(Stream, Result, ByteVal, Error, !IO).

:- pred read_byte_val_2(stream::in, result_code::out, int::out,
    system_error::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    read_byte_val_2(Stream::in, Result::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    int b = mercury_get_byte(Stream);
    if (b == EOF) {
        if (MR_FERROR(*Stream)) {
            Result = ML_RESULT_CODE_ERROR;
            Error = errno;
        } else {
            Result = ML_RESULT_CODE_EOF;
            Error = 0;
        }
        ByteVal = 0;
    } else {
        Result = ML_RESULT_CODE_OK;
        ByteVal = b;
        Error = 0;
    }
").

putback_char(input_stream(Stream), Character, !IO) :-
    putback_char_2(Stream, Character, Ok, !IO),
    (
        Ok = yes
    ;
        Ok = no,
        throw(io_error("failed to put back character"))
    ).

:- pred putback_char_2(stream::in, char::in, bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    putback_char_2(Stream::in, Character::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing, may_not_duplicate],
"
    MercuryFilePtr mf = Stream;
    Ok = MR_TRUE;
    if (Character <= 0x7f) {
        if (MR_UNGETCH(*mf, Character) == EOF) {
            Ok = MR_FALSE;
        } else {
            if (Character == '\\n') {
                MR_line_number(*mf)--;
            }
        }
    } else {
        /* This requires multiple pushback in the underlying C library. */
        char        buf[5];
        ML_ssize_t  len;
        len = MR_utf8_encode(buf, Character);
        for (; len > 0; len--) {
            if (MR_UNGETCH(*mf, buf[len - 1]) == EOF) {
                Ok = MR_FALSE;
                break;
            }
        }
    }
").

putback_byte(binary_input_stream(Stream), Character, !IO) :-
    putback_byte_2(Stream, Character, Ok, !IO),
    (
        Ok = yes
    ;
        Ok = no,
        throw(io_error("failed to put back byte"))
    ).

:- pred putback_byte_2(stream::in, int::in, bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    putback_byte_2(Stream::in, Character::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr mf = Stream;
    if (MR_UNGETCH(*mf, Character) == EOF) {
        Ok = MR_FALSE;
    } else {
        Ok = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    read_char_code_2(File::in, Result::out, Char::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    try {
        int c = io.mercury_getc(mf);
        if (c == -1) {
            Result = io.ML_RESULT_CODE_EOF;
            Char = 0;
        } else {
            Result = io.ML_RESULT_CODE_OK;
            Char = c;
        }
        Error = null;
    } catch (System.Exception e) {
        Result = io.ML_RESULT_CODE_ERROR;
        Char = 0;
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    read_byte_val_2(File::in, Result::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback != -1) {
        Result = io.ML_RESULT_CODE_OK;
        ByteVal = mf.putback;
        Error = null;
        mf.putback = -1;
    } else {
        try {
            int b = mf.stream.ReadByte();
            if (b == -1) {
                Result = io.ML_RESULT_CODE_EOF;
                ByteVal = 0;
            } else {
                Result = io.ML_RESULT_CODE_OK;
                ByteVal = b;
            }
            Error = null;
        } catch (System.Exception e) {
            Result = io.ML_RESULT_CODE_ERROR;
            ByteVal = 0;
            Error = e;
        }
    }
").

:- pragma foreign_proc("C#",
    putback_char_2(File::in, Character::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback == -1) {
        mf.putback = Character;
        if (Character == '\\n') {
            mf.line_number--;
        }
        Ok = mr_bool.YES;
    } else {
        Ok = mr_bool.NO;
    }
").

:- pragma foreign_proc("C#",
    putback_byte_2(File::in, Byte::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback == -1) {
        mf.putback = Byte;
        Ok = mr_bool.YES;
    } else {
        Ok = mr_bool.NO;
    }
").

:- pragma foreign_proc("Java",
    read_char_code_2(File::in, Result::out, CharCode::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        int c = ((io.MR_TextInputFile) File).read_char();
        if (c == -1) {
            Result = io.ML_RESULT_CODE_EOF;
            CharCode = 0;
        } else {
            Result = io.ML_RESULT_CODE_OK;
            CharCode = c;
        }
        Error = null;
    } catch (java.io.IOException e) {
        Result = io.ML_RESULT_CODE_ERROR;
        CharCode = 0;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    read_byte_val_2(File::in, Result::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        int b = ((io.MR_BinaryInputFile) File).read_byte();
        if (b == -1) {
            Result = io.ML_RESULT_CODE_EOF;
            ByteVal = 0;
        } else {
            Result = io.ML_RESULT_CODE_OK;
            ByteVal = b;
        }
        Error = null;
    } catch (java.io.IOException e) {
        Result = io.ML_RESULT_CODE_ERROR;
        ByteVal = 0;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    putback_char_2(File::in, Character::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ((io.MR_TextInputFile) File).ungetc(Character);
    Ok = bool.YES;
").

:- pragma foreign_proc("Java",
    putback_byte_2(File::in, Byte::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ((io.MR_BinaryInputFile) File).ungetc((byte) Byte);
    Ok = bool.YES;
").

:- pragma foreign_proc("Erlang",
    read_char_code_2(Stream::in, Result::out, Char::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
    case mercury__io:mercury_getc(Stream) of
        C when is_integer(C) ->
            Result = {ok},
            Char = C,
            Error = ok;
        eof ->
            Result = {eof},
            Char = 0,
            Error = ok;
        {error, Reason} ->
            Result = {error},
            Char = 0,
            Error = {error, Reason}
    end
").

:- pragma foreign_proc("Erlang",
    read_byte_val_2(Stream::in, Result::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
    case mercury__io:mercury_getc(Stream) of
        B when is_integer(B) ->
            Result = {ok},
            ByteVal = B,
            Error = ok;
        eof ->
            Result = {eof},
            ByteVal = 0,
            Error = ok;
        {error, Reason} ->
            Result = {error},
            ByteVal = 0,
            Error = {error, Reason}
    end
").

:- pragma foreign_proc("Erlang",
    putback_char_2(File::in, Character::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury__io:mercury_putback(File, Character),
    Ok = {yes}
").

:- pragma foreign_proc("Erlang",
    putback_byte_2(File::in, Byte::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury__io:mercury_putback(File, Byte),
    Ok = {yes}
").

%---------------------------------------------------------------------------%
%
% Output predicates (with output to mercury_current_text_output).
%

write_string(Message, !IO) :-
    output_stream(Stream, !IO),
    write_string(Stream, Message, !IO).

write_char(Character, !IO) :-
    output_stream(Stream, !IO),
    write_char(Stream, Character, !IO).

write_int(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int(Stream, Val, !IO).

write_int8(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int8(Stream, Val, !IO).

write_int16(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int16(Stream, Val, !IO).

write_int32(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int32(Stream, Val, !IO).

write_uint(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint(Stream, Val, !IO).

write_uint8(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint8(Stream, Val, !IO).

write_uint16(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint16(Stream, Val, !IO).

write_uint32(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint32(Stream, Val, !IO).

write_float(Val, !IO) :-
    output_stream(Stream, !IO),
    write_float(Stream, Val, !IO).

%---------------------------------------------------------------------------%
%
% Output predicates (with output to mercury_current_binary_output).
%

write_byte(Byte, !IO) :-
    binary_output_stream(Stream, !IO),
    write_byte(Stream, Byte, !IO).

write_binary_int8(Int8, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_int8(Stream, Int8, !IO).

write_binary_uint8(UInt8, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint8(Stream, UInt8, !IO).

write_bitmap(Bitmap, !IO) :-
    binary_output_stream(Stream, !IO),
    write_bitmap(Stream, Bitmap, !IO).

write_bitmap(Bitmap, Start, NumBytes, !IO) :-
    binary_output_stream(Stream, !IO),
    write_bitmap(Stream, Bitmap, Start, NumBytes, !IO).

flush_output(!IO) :-
    output_stream(Stream, !IO),
    flush_output(Stream, !IO).

flush_binary_output(!IO) :-
    binary_output_stream(Stream, !IO),
    flush_binary_output(Stream, !IO).

%---------------------------------------------------------------------------%
%
% Moving about binary streams.
%

:- pred whence_to_int(io.whence::in, int::out) is det.

whence_to_int(set, 0).
whence_to_int(cur, 1).
whence_to_int(end, 2).

seek_binary_input(binary_input_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    seek_binary_2(Stream, Flag, Offset, Error, !IO),
    throw_on_error(Error, "error seeking in file: ", !IO).

seek_binary_output(binary_output_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    seek_binary_2(Stream, Flag, Offset, Error, !IO),
    throw_on_error(Error, "error seeking in file: ", !IO).

:- pred seek_binary_2(stream::in, int::in, int::in, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    seek_binary_2(Stream::in, Flag::in, Off::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };

    /* XXX check if the stream is seekable */
    if (MR_IS_FILE_STREAM(*Stream)) {
        if (fseek(MR_file(*Stream), Off, seek_flags[Flag]) < 0) {
            Error = errno;
        } else {
            Error = 0;
        }
    } else {
        Error = EINVAL;
    }
").

binary_input_stream_offset(binary_input_stream(Stream), Offset, !IO) :-
    binary_stream_offset_2(Stream, Offset, Error, !IO),
    throw_on_error(Error, "error getting file offset: ", !IO).

binary_output_stream_offset(binary_output_stream(Stream), Offset, !IO) :-
    binary_stream_offset_2(Stream, Offset, Error, !IO),
    throw_on_error(Error, "error getting file offset: ", !IO).

:- pred binary_stream_offset_2(stream::in, int::out, system_error::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    binary_stream_offset_2(Stream::in, Offset::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /* XXX should check if the stream is tellable */
    if (MR_IS_FILE_STREAM(*Stream)) {
        Offset = ftell(MR_file(*Stream));
        if (Offset < 0) {
            Error = errno;
        } else {
            Error = 0;
        }
    } else {
        Error = EINVAL;
    }
").

%---------------------------------------------------------------------------%
%
% Output predicates (with output to the specified stream).
%

write_string(output_stream(Stream), Message, !IO) :-
    do_write_string(Stream, Message, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_string(stream::in, string::in, system_error::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    do_write_string(Stream::in, Message::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    const char *s = Message;
    if (ML_fprintf(Stream, ""%s"", s) < 0) {
        Error = errno;
    } else {
        Error = 0;
        while (*s) {
            if (*s++ == '\\n') {
                MR_line_number(*Stream)++;
            }
        }
    }
").

write_char(output_stream(Stream), Character, !IO) :-
    do_write_char(Stream, Character, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_char(stream::in, char::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_char(Stream::in, Character::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    Error = 0;
    if (Character <= 0x7f) {
        if (MR_PUTCH(*Stream, Character) < 0) {
            Error = errno;
        } else if (Character == '\\n') {
            MR_line_number(*Stream)++;
        }
    } else {
        char    buf[5];
        size_t  len;
        size_t  i;
        len = MR_utf8_encode(buf, Character);
        for (i = 0; i < len; i++) {
            if (MR_PUTCH(*Stream, buf[i]) < 0) {
                Error = errno;
                break;
            }
        }
    }
").

write_int(output_stream(Stream), Val, !IO) :-
    do_write_int(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_int(stream::in, int::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_int(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" MR_INTEGER_LENGTH_MODIFIER ""d"", Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_int8(output_stream(Stream), Val, !IO) :-
    do_write_int8(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_int8(stream::in, int8::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_int8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRId8, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_int16(output_stream(Stream), Val, !IO) :-
    do_write_int16(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_int16(stream::in, int16::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_int16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRId16, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_int32(output_stream(Stream), Val, !IO) :-
    do_write_int32(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_int32(stream::in, int32::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_int32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRId32, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_uint(output_stream(Stream), Val, !IO) :-
    do_write_uint(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_uint(stream::in, uint::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_uint(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" MR_INTEGER_LENGTH_MODIFIER ""u"", Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_uint8(output_stream(Stream), Val, !IO) :-
    do_write_uint8(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_uint8(stream::in, uint8::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_uint8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRIu8, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_uint16(output_stream(Stream), Val, !IO) :-
    do_write_uint16(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_uint16(stream::in, uint16::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_uint16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRIu16, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_uint32(output_stream(Stream), Val, !IO) :-
    do_write_uint32(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_uint32(stream::in, uint32::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_uint32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRIu32, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_float(output_stream(Stream), Val, !IO) :-
    do_write_float(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_float(stream::in, float::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_float(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    char buf[MR_SPRINTF_FLOAT_BUF_SIZE];
    MR_sprintf_float(buf, Val);
    if (ML_fprintf(Stream, ""%s"", buf) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_byte(binary_output_stream(Stream), Byte, !IO) :-
    do_write_byte(Stream, Byte, Error, !IO),
    throw_on_output_error(Error, !IO).

write_binary_int8(binary_output_stream(Stream), Int8, !IO) :-
    Int = int8.to_int(Int8),
    do_write_byte(Stream, Int, Error, !IO),
    throw_on_output_error(Error, !IO).

write_binary_uint8(binary_output_stream(Stream), UInt8, !IO) :-
    Int = uint8.to_int(UInt8),
    do_write_byte(Stream, Int, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred do_write_byte(stream::in, int::in, system_error::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    do_write_byte(Stream::in, Byte::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    /* call putc with a strictly non-negative byte-sized integer */
    if (MR_PUTCH(*Stream, (int) ((unsigned char) Byte)) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

write_bitmap(binary_output_stream(Stream), Bitmap, !IO) :-
    ( if NumBytes = Bitmap ^ num_bytes then
        do_write_bitmap(Stream, Bitmap, 0, NumBytes, Error, !IO),
        throw_on_output_error(Error, !IO)
    else
        error("io.write_bitmap: bitmap contains partial final byte")
    ).

write_bitmap(binary_output_stream(Stream), Bitmap, Start, NumBytes, !IO) :-
    ( if NumBytes = 0 then
        true
    else if
        NumBytes > 0,
        byte_in_range(Bitmap, Start),
        byte_in_range(Bitmap, Start + NumBytes - 1)
    then
        do_write_bitmap(Stream, Bitmap, Start, NumBytes, Error, !IO),
        throw_on_output_error(Error, !IO)
    else
        bitmap.throw_bounds_error(Bitmap, "io.write_bitmap",
            Start * bits_per_byte, NumBytes * bits_per_byte)
    ).

:- pred do_write_bitmap(stream, bitmap, int, int, system_error, io, io).
%:- mode do_write_bitmap(in, bitmap_ui, in, in, out, di, uo) is det.
:- mode do_write_bitmap(in, in, in, in, out, di, uo) is det.

:- pragma foreign_proc("C",
    do_write_bitmap(Stream::in, Bitmap::in, Start::in, Length::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        no_sharing],
"
    MR_Integer bytes_written =
        (MR_Integer)MR_WRITE(*Stream, Bitmap->elements + Start, Length);
    if (bytes_written != Length) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_bitmap(Stream::in, Bitmap::in, Start::in, Length::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        no_sharing],
"
    try {
        Stream.stream.Write(Bitmap.elements, Start, Length);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_bitmap(Stream::in, Bitmap::in, Start::in, Length::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        no_sharing],
"
    try {
        ((MR_BinaryOutputFile) Stream).write(Bitmap.elements, Start, Length);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    do_write_bitmap(Stream::in, Bitmap::in, Start::in, Length::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        no_sharing],
"
    {Binary, _NumBits} = Bitmap,
    <<_:Start/binary, Mid:Length/binary, _/binary>> = Binary,
    mercury__io:mercury_write_binary(Stream, Mid),
    % mercury_write_binary does not return errors yet.
    Error = ok
").

flush_output(output_stream(Stream), !IO) :-
    flush_output_2(Stream, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred flush_output_2(stream::in, system_error::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    flush_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (MR_FLUSH(*Stream) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

flush_binary_output(binary_output_stream(Stream), !IO) :-
    flush_binary_output_2(Stream, Error, !IO),
    throw_on_output_error(Error, !IO).

:- pred flush_binary_output_2(stream::in, system_error::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    flush_binary_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (MR_FLUSH(*Stream) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

% C# missing seek_binary_2
% C# missing binary_stream_offset_2

:- pragma foreign_proc("C#",
    do_write_string(Stream::in, Message::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Message);
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_char(Stream::in, Character::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    io.MR_MercuryFileStruct stream = Stream;
    try {
        // See mercury_print_string().
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
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_int(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_uint(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_int8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_int16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_int32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_uint8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_uint16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_uint32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_byte(Stream::in, Byte::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Stream.stream.WriteByte(System.Convert.ToByte(Byte));
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    flush_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Stream.stream.Flush();
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    flush_binary_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Stream.stream.Flush();
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    seek_binary_2(Stream::in, Flag::in, Off::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_BinaryFile) Stream).seek_binary(Flag, Off);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    binary_stream_offset_2(Stream::in, Offset::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Offset = ((io.MR_BinaryFile) Stream).getOffset();
        Error = null;
    } catch (java.io.IOException e) {
        Offset = -1;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_string(Stream::in, Message::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(Message);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_char(Stream::in, Character::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        char[] buf = java.lang.Character.toChars(Character);
        for (char c : buf) {
            ((io.MR_TextOutputFile) Stream).put(c);
        }
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_int(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(String.valueOf(Val));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_int8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(String.valueOf(Val));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_int16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(String.valueOf(Val));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_int32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(String.valueOf(Val));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_uint(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(
            java.lang.Long.toString(Val & 0xffffffffL));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_uint8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(
            java.lang.Integer.toString(Val & 0xff));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_uint16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(
            java.lang.Integer.toString(Val & 0xffff));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_uint32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(
            java.lang.Long.toString(Val & 0xffffffffL));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_float(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    io.MR_TextOutputFile stream = (io.MR_TextOutputFile) Stream;

    try {
        if (Double.isNaN(Val)) {
            stream.write(""nan"");
        } else if (Double.isInfinite(Val)) {
            if (Val < 0.0) {
                stream.write(""-infinity"");
            } else {
                stream.write(""infinity"");
            }
        } else {
            stream.write(Double.toString(Val));
        }
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_byte(Stream::in, Byte::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ((io.MR_BinaryOutputFile) Stream).put((byte) Byte);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    flush_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ((io.MR_TextOutputFile) Stream).flush();
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    flush_binary_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ((io.MR_BinaryOutputFile) Stream).flush();
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    seek_binary_2(Stream::in, Flag::in, Off::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    % Constants from whence_to_int.
    case Flag of
        0 -> Loc = {bof, Off};
        1 -> Loc = {cur, Off};
        2 -> Loc = {eof, Off}
    end,
    case mercury__io:mercury_set_pos(Stream, Loc) of
        {ok, _NewPosition} ->
            Error = ok;
        {error, Reason} ->
            Error = {error, Reason}
    end
").

:- pragma foreign_proc("Erlang",
    binary_stream_offset_2(Stream::in, Offset::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    case mercury__io:mercury_get_pos(Stream) of
        {ok, Offset} ->
            Error = ok;
        {error, Reason} ->
            Offset = -1,
            Error = {error, Reason}
    end
").

:- pragma foreign_proc("Erlang",
    do_write_char(Stream::in, Character::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    mercury__io:mercury_write_char(Stream, Character),
    % mercury_write_char does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_int(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_uint(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_int8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_int16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_int32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_uint8(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_uint16(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_uint32(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    mercury__io:mercury_write_int(Stream, Val),
    % mercury_write_int does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_string(Stream::in, Message::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    mercury__io:mercury_write_string(Stream, Message),
    % mercury_write_string does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    do_write_byte(Stream::in, Byte::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    mercury__io:mercury_write_char(Stream, Byte),
    % mercury_write_char does not return errors yet.
    Error = ok
").

:- pragma foreign_proc("Erlang",
    flush_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Error = mercury__io:mercury_sync(Stream)
").

:- pragma foreign_proc("Erlang",
    flush_binary_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Error = mercury__io:mercury_sync(Stream)
").

do_write_float(Stream, Float, Error, !IO) :-
    do_write_string(Stream, string.float_to_string(Float), Error, !IO).

%---------------------------------------------------------------------------%
%
% Stream predicates.
%

:- pragma foreign_export("C", stdin_stream_2(out, di, uo),
    "ML_io_stdin_stream").
:- pragma foreign_export("C", stdout_stream_2(out, di, uo),
    "ML_io_stdout_stream").
:- pragma foreign_export("C", stderr_stream_2(out, di, uo),
    "ML_io_stderr_stream").

stdin_stream = input_stream(stdin_stream_2).

:- func stdin_stream_2 = stream.
:- pragma foreign_proc("C",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdin;
").

:- pragma foreign_proc("C#",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdin;
").

:- pragma foreign_proc("Java",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdin;
").

stdin_stream(input_stream(Stream), !IO) :-
    stdin_stream_2(Stream, !IO).

:- pred stdin_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdin;
").

stdout_stream = output_stream(io.stdout_stream_2).

:- func stdout_stream_2 = stream.
:- pragma foreign_proc("C",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdout;
").

:- pragma foreign_proc("C#",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdout;
").

:- pragma foreign_proc("Java",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stdout;
").

stdout_stream(output_stream(Stream), !IO) :-
    stdout_stream_2(Stream, !IO).

:- pred stdout_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdout;
").

stderr_stream = output_stream(stderr_stream_2).

:- func stderr_stream_2 = stream.
:- pragma foreign_proc("C",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stderr;
").

:- pragma foreign_proc("C#",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stderr;
").

:- pragma foreign_proc("Java",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = io.mercury_stderr;
").

stderr_stream(output_stream(Stream), !IO) :-
    stderr_stream_2(Stream, !IO).

:- pred stderr_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stderr;
").

stdin_binary_stream(binary_input_stream(Stream), !IO) :-
    stdin_binary_stream_2(Stream, !IO).

:- pred stdin_binary_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdin_binary;
").

stdout_binary_stream(binary_output_stream(Stream), !IO) :-
    stdout_binary_stream_2(Stream, !IO).

:- pred stdout_binary_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = &mercury_stdout_binary;
").

input_stream(input_stream(Stream), !IO) :-
    input_stream_2(Stream, !IO).

:- pred input_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_text_input();
").

output_stream(output_stream(Stream), !IO) :-
    output_stream_2(Stream, !IO).

:- pred output_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_text_output();
").

binary_input_stream(binary_input_stream(Stream), !IO) :-
    binary_input_stream_2(Stream, !IO).

:- pred binary_input_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_binary_input();
").

binary_output_stream(binary_output_stream(Stream), !IO) :-
    binary_output_stream_2(Stream, !IO).

:- pred binary_output_stream_2(stream::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    Stream = mercury_current_binary_output();
").

:- pragma foreign_proc("C",
    get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*mercury_current_text_input());
").

get_line_number(input_stream(Stream), LineNum, !IO) :-
    get_line_number_2(Stream, LineNum, !IO).

:- pred get_line_number_2(stream::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*Stream);
").

:- pragma foreign_proc("C",
    set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*mercury_current_text_input()) = LineNum;
").

set_line_number(input_stream(Stream), LineNum, !IO) :-
    set_line_number_2(Stream, LineNum,!IO).

:- pred set_line_number_2(stream::in, int::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*Stream) = LineNum;
").

:- pragma foreign_proc("C",
    get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*mercury_current_text_output());
").

get_output_line_number(output_stream(Stream), LineNum, !IO) :-
    get_output_line_number_2(Stream, LineNum, !IO).

:- pred get_output_line_number_2(stream::in, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*Stream);
").

:- pragma foreign_proc("C",
    set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*mercury_current_text_output()) = LineNum;
").

set_output_line_number(output_stream(Stream), LineNum, !IO) :-
    set_output_line_number_2(Stream, LineNum, !IO).

:- pred set_output_line_number_2(stream::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*Stream) = LineNum;
").

set_input_stream(input_stream(NewStream), input_stream(OutStream), !IO) :-
    set_input_stream_2(NewStream, OutStream, !IO).

:- pred set_input_stream_2(stream::in, stream::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_text_input();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_text_input_index);
").

set_output_stream(output_stream(NewStream), output_stream(OutStream),
        !IO) :-
    set_output_stream_2(NewStream, OutStream, !IO).

:- pred set_output_stream_2(stream::in, stream::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_text_output();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_text_output_index);
").

set_binary_input_stream(binary_input_stream(NewStream),
        binary_input_stream(OutStream), !IO) :-
    set_binary_input_stream_2(NewStream, OutStream, !IO).

:- pred set_binary_input_stream_2(stream::in, stream::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_binary_input();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_binary_input_index);
").

set_binary_output_stream(binary_output_stream(NewStream),
        binary_output_stream(OutStream), !IO) :-
    set_binary_output_stream_2(NewStream, OutStream, !IO).

:- pred set_binary_output_stream_2(stream::in, stream::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing is okay as io.stream is a foreign type so can't be reused.
"
    OutStream = mercury_current_binary_output();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_binary_output_index);
").

:- pragma foreign_proc("C#",
    stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdin;
").

:- pragma foreign_proc("C#",
    stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdout;
").

:- pragma foreign_proc("C#",
    stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stderr;
").

:- pragma foreign_proc("C#",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdin_binary;
").

:- pragma foreign_proc("C#",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdout_binary;
").

:- pragma foreign_proc("C#",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_text_input;
").

:- pragma foreign_proc("C#",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_text_output;
").

:- pragma foreign_proc("C#",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_binary_input;
").

:- pragma foreign_proc("C#",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = io.mercury_current_binary_output;
").

:- pragma foreign_proc("C#",
    get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_input.line_number;
").

:- pragma foreign_proc("C#",
    get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = Stream.line_number;
").

:- pragma foreign_proc("C#",
    set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_input.line_number = LineNum;
").

:- pragma foreign_proc("C#",
    set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
    get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_output.line_number;
").

:- pragma foreign_proc("C#",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").

:- pragma foreign_proc("C#",
    set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_output.line_number = LineNum;
").

:- pragma foreign_proc("C#",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").

:- pragma foreign_proc("C#",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_input;
    io.mercury_current_text_input = NewStream;
").

:- pragma foreign_proc("C#",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_output;
    io.mercury_current_text_output = NewStream;
").

:- pragma foreign_proc("C#",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_input;
    io.mercury_current_binary_input = NewStream;
").

:- pragma foreign_proc("C#",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_output;
    io.mercury_current_binary_output = NewStream;
").

:- pragma foreign_proc("Java",
    stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdin;
").

:- pragma foreign_proc("Java",
    stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stdout;
").

:- pragma foreign_proc("Java",
    stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = io.mercury_stderr;
").

:- pragma foreign_proc("Java",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    io.ensure_init_mercury_stdin_binary();
    Stream = io.mercury_stdin_binary;
").

:- pragma foreign_proc("Java",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    io.ensure_init_mercury_stdout_binary();
    Stream = io.mercury_stdout_binary;
").

:- pragma foreign_proc("Java",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_text_input.get();
").

:- pragma foreign_proc("Java",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_text_output.get();
").

:- pragma foreign_proc("Java",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_binary_input.get();
").

:- pragma foreign_proc("Java",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = io.mercury_current_binary_output.get();
").

:- pragma foreign_proc("Java",
    get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_input.get().line_number;
").

:- pragma foreign_proc("Java",
    get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = ((io.MR_TextInputFile) Stream).line_number;
").

:- pragma foreign_proc("Java",
    set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_input.get().line_number = LineNum;
").

:- pragma foreign_proc("Java",
    set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ((io.MR_TextInputFile) Stream).line_number = LineNum;
").

:- pragma foreign_proc("Java",
    get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = io.mercury_current_text_output.get().line_number;
").

:- pragma foreign_proc("Java",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = ((io.MR_TextOutputFile) Stream).line_number;
").

:- pragma foreign_proc("Java",
    set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.mercury_current_text_output.get().line_number = LineNum;
").

:- pragma foreign_proc("Java",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ((io.MR_TextOutputFile) Stream).line_number = LineNum;
").

    % io.set_input_stream(NewStream, OldStream, IO0, IO1)
    % Changes the current input stream to the stream specified.
    % Returns the previous stream.

:- pragma foreign_proc("Java",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_input.get();
    io.mercury_current_text_input.set((io.MR_TextInputFile) NewStream);
").

:- pragma foreign_proc("Java",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_text_output.get();
    io.mercury_current_text_output.set((io.MR_TextOutputFile) NewStream);
").

:- pragma foreign_proc("Java",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_input.get();
    io.mercury_current_binary_input.set((io.MR_BinaryInputFile) NewStream);
").

:- pragma foreign_proc("Java",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = io.mercury_current_binary_output.get();
    io.mercury_current_binary_output.set((io.MR_BinaryOutputFile) NewStream);
").

:- pragma foreign_proc("Erlang",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdin_stream')
").

:- pragma foreign_proc("Erlang",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdout_stream')
").

:- pragma foreign_proc("Erlang",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stderr_stream')
").

:- pragma foreign_proc("Erlang",
    stdin_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdin_stream')
").

:- pragma foreign_proc("Erlang",
    stdout_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdout_stream')
").

:- pragma foreign_proc("Erlang",
    stderr_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stderr_stream')
").

:- pragma foreign_proc("Erlang",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdin_binary_stream')
").

:- pragma foreign_proc("Erlang",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = get('ML_stdout_binary_stream')
").

:- pragma foreign_proc("Erlang",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_input
").

:- pragma foreign_proc("Erlang",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_output
").

:- pragma foreign_proc("Erlang",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_binary_input
").

:- pragma foreign_proc("Erlang",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_binary_output
").

:- pragma foreign_proc("Erlang",
    get_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_input,
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    get_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    set_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_input,
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    set_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    get_output_line_number(LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_output,
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = mercury__io:mercury_get_line_number(Stream)
").

:- pragma foreign_proc("Erlang",
    set_output_line_number(LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = ?ML_get_current_text_output,
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury__io:mercury_set_line_number(Stream, LineNum)
").

:- pragma foreign_proc("Erlang",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_text_input,
    mercury__io:mercury_set_current_text_input(NewStream)
").

:- pragma foreign_proc("Erlang",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_text_output,
    mercury__io:mercury_set_current_text_output(NewStream)
").

:- pragma foreign_proc("Erlang",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_binary_input,
    mercury__io:mercury_set_current_binary_input(NewStream)
").

:- pragma foreign_proc("Erlang",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = ?ML_get_current_binary_output,
    mercury__io:mercury_set_current_binary_output(NewStream)
").

% Predicates to temporarily change the input/output stream.
% XXX We should not need these if we passed streams explicitly everywhere.

:- pred with_input_stream(input_stream, pred(T, io, io), T, io, io).
:- mode with_input_stream(in, pred(out, di, uo) is det, out, di, uo) is det.
:- mode with_input_stream(in, pred(out, di, uo) is cc_multi, out, di, uo)
    is cc_multi.

with_input_stream(Stream, Pred, Result, !IO) :-
    set_input_stream(Stream, OrigStream, !IO),
    finally(Pred, Result,
        restore_input_stream(Pred, OrigStream), _CleanupRes, !IO).

:- pred with_output_stream(output_stream, pred(io, io), io, io).
:- mode with_output_stream(in, pred(di, uo) is det, di, uo) is det.
:- mode with_output_stream(in, pred(di, uo) is cc_multi, di, uo) is cc_multi.

with_output_stream(Stream, Pred, !IO) :-
    set_output_stream(Stream, OrigStream, !IO),
    finally(call_pred_no_result(Pred), _Result,
        restore_output_stream(Pred, OrigStream), _CleanupRes, !IO).

:- pred call_pred_no_result(pred(io, io), {}, io, io).
:- mode call_pred_no_result(pred(di, uo) is det, out, di, uo) is det.
:- mode call_pred_no_result(pred(di, uo) is cc_multi, out, di, uo)
    is cc_multi.

call_pred_no_result(Pred, {}, !IO) :-
    Pred(!IO).

:- pragma no_determinism_warning(restore_input_stream/5).
:- pred restore_input_stream(pred(T, io, io), input_stream, io.res, io, io).
:- mode restore_input_stream(pred(out, di, uo) is det, in, out, di, uo)
    is det.
:- mode restore_input_stream(pred(out, di, uo) is cc_multi, in, out, di, uo)
    is cc_multi.

restore_input_stream(_DummyPred, Stream, ok, !IO) :-
    set_input_stream(Stream, _OldStream, !IO).

:- pragma no_determinism_warning(restore_output_stream/5).
:- pred restore_output_stream(pred(io, io), output_stream, io.res, io, io).
:- mode restore_output_stream(pred(di, uo) is det, in, out, di, uo) is det.
:- mode restore_output_stream(pred(di, uo) is cc_multi, in, out, di, uo)
    is cc_multi.

restore_output_stream(_DummyPred, Stream, ok, !IO) :-
    set_output_stream(Stream, _OldStream, !IO).

% Stream open/close predicates.

    % do_open_binary(File, Mode, StreamId, Stream, Error, !IO):
    % do_open_text(File, Mode, StreamId, Stream, Error, !IO):
    %
    % Attempts to open a file in the specified mode.
    % The Mode is a string suitable for passing to fopen().
    % StreamId is a unique integer identifying the open.
    % StreamId and Stream are valid only if Error indicates an error occurred.
    %
:- pred do_open_binary(string::in, string::in, int::out, stream::out,
    system_error::out, io::di, io::uo) is det.

:- pred do_open_text(string::in, string::in, int::out, stream::out,
    system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_open_text(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    Stream = mercury_open(FileName, Mode, MR_ALLOC_ID);
    if (Stream != NULL) {
        StreamId = mercury_next_stream_id();
        Error = 0;
    } else {
        StreamId = -1;
        Error = errno;
    }
").

:- pragma foreign_proc("C",
    do_open_binary(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    Stream = mercury_open(FileName, Mode, MR_ALLOC_ID);
    if (Stream != NULL) {
        StreamId = mercury_next_stream_id();
        Error = 0;
    } else {
        StreamId = -1;
        Error = errno;
    }
").

:- pragma foreign_proc("C#",
    do_open_text(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Stream = io.mercury_open(FileName, Mode, io.ML_default_line_ending);
        StreamId = Stream.id;
        Error = null;
    } catch (System.Exception e) {
        StreamId = -1;
        Stream = null;
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_open_binary(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Stream = io.mercury_open(FileName, Mode,
            io.ML_line_ending_kind.ML_raw_binary);
        StreamId = Stream.id;
        Error = null;
    } catch (System.Exception e) {
        StreamId = -1;
        Stream = null;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_open_text(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        switch (Mode.charAt(0)) {
            case 'r':
                Stream = new MR_TextInputFile(
                    new java.io.FileInputStream(FileName));
                break;
            case 'w':
                Stream = new MR_TextOutputFile(
                    new java.io.FileOutputStream(FileName));
                break;
            case 'a':
                Stream = new MR_TextOutputFile(
                    new java.io.FileOutputStream(FileName, true));
                break;
            default:
                throw new RuntimeException(""Invalid file opening mode: "" +
                    Mode);
        }
        StreamId = Stream.id;
        Error = null;
    } catch (java.lang.Exception e) {
        Stream = null;
        StreamId = -1;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_open_binary(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
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
                throw new RuntimeException(""Invalid file opening mode: "" +
                    Mode);
        }
        StreamId = Stream.id;
        Error = null;
    } catch (java.lang.Exception e) {
        Stream = null;
        StreamId = -1;
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    do_open_text(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    FileNameStr = binary_to_list(FileName),
    ModeStr = binary_to_list(Mode),
    % XXX This should probably pass encoding 'utf8'.
    case mercury__io:mercury_open_stream(FileNameStr, ModeStr) of
        {ok, Stream} ->
            {'ML_stream', StreamId, _Pid} = Stream,
            Error = ok;
        {error, Reason} ->
            StreamId = -1,
            Stream = null,
            Error = {error, Reason}
    end
").

:- pragma foreign_proc("Erlang",
    do_open_binary(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    FileNameStr = binary_to_list(FileName),
    ModeStr = binary_to_list(Mode),
    % XXX This should probably pass encoding 'latin1'.
    case mercury__io:mercury_open_stream(FileNameStr, ModeStr) of
        {ok, Stream} ->
            {'ML_stream', StreamId, _Pid} = Stream,
            Error = ok;
        {error, Reason} ->
            StreamId = -1,
            Stream = null,
            Error = {error, Reason}
    end
").

close_input(input_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

close_output(output_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

close_binary_input(binary_input_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

close_binary_output(binary_output_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

:- pred close_stream(stream::in, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_stream(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (mercury_close(Stream) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    close_stream(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        io.mercury_close(Stream);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    close_stream(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Stream.close();
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    close_stream(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Error = mercury__io:mercury_close_stream(Stream)
").

%---------------------------------------------------------------------------%
%
% Miscellaneous predicates
%
% XXX Grouping by language is more confusing than helpful.
%

    % Fallback implementation.
progname(DefaultProgName, ProgName, !IO) :-
    ProgName = DefaultProgName.

:- pragma foreign_proc("C",
    progname(DefaultProgname::in, PrognameOut::out, _IO0::di, _IO::uo),
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
    command_line_arguments(Args::out, _IO0::di, _IO::uo),
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
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ExitStatus = mercury_exit_status;
").

:- pragma foreign_proc("C",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    mercury_exit_status = (int) ExitStatus;
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
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
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
        Error = errno;
    } else {
        /* Wait for the spawned process to exit. */
        do {
            err = waitpid(pid, &st, 0);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == -1) {
            Error = errno;
        } else {
            Status = st;
            Error = 0;
        }
    }

#else   /* !MR_THREAD_SAFE || !MR_HAVE_POSIX_SPAWN || !MR_HAVE_ENVIRON */

  #ifdef MR_WIN32
    Status = _wsystem(ML_utf8_to_wide(Command));
  #else
    Status = system(Command);
  #endif

    if (Status == -1) {
        Error = errno;
    } else {
        Error = 0;
    }

#endif  /* !MR_THREAD_SAFE || !MR_HAVE_POSIX_SPAWN || !MR_HAVE_ENVIRON */
").

decode_system_command_exit_code(Code0) = Status :-
    decode_system_command_exit_code(Code0, Exited, ExitCode, Signalled,
        Signal),
    (
        Exited = yes,
        Status = ok(exited(ExitCode))
    ;
        Exited = no,
        (
            Signalled = yes,
            Status = ok(signalled(Signal))
        ;
            Signalled = no,
            Status = error(io_error("unknown result code from system command"))
        )
    ).

    % Interpret the child process exit status returned by system() or wait():
    %
:- pred decode_system_command_exit_code(int::in, bool::out, int::out,
    bool::out, int::out) is det.

% This is a fall-back for back-ends that don't support the C interface.
decode_system_command_exit_code(Status, yes, Status, no, 0).

:- pragma foreign_proc("C",
    decode_system_command_exit_code(Status0::in, Exited::out, Status::out,
        Signalled::out, Signal::out),
    [will_not_call_mercury, thread_safe, promise_pure,
        does_not_affect_liveness, no_sharing],
"
    #if defined (WIFEXITED) && defined (WEXITSTATUS) && \
            defined (WIFSIGNALED) && defined (WTERMSIG)
        if (WIFEXITED(Status0)) {
            Exited = MR_YES;
            Signalled = MR_NO;
            Status = WEXITSTATUS(Status0);
        } else if (WIFSIGNALED(Status0)) {
            Exited = MR_NO;
            Signalled = MR_YES;
            Signal = -WTERMSIG(Status0);
        } else {
            Exited = MR_NO;
            Signalled = MR_NO;
        }
    #else
        Exited = MR_YES;
        Status = Status0;
        Signalled = MR_NO;
    #endif
").

%---------------------%

:- pragma foreign_proc("C#",
    command_line_arguments(Args::out, _IO0::di, _IO::uo),
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
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ExitStatus = System.Environment.ExitCode;
").

:- pragma foreign_proc("C#",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    System.Environment.ExitCode = ExitStatus;
").

:- pragma foreign_proc("C#",
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
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

        System.Diagnostics.Process process = new System.Diagnostics.Process();
        // Never interpret the command as a document to open with whatever
        // application is registered for that document type. This also
        // prevents creating a new window for console programs on Windows.
        process.StartInfo.UseShellExecute = false;
        process.StartInfo.FileName = command;
        process.StartInfo.Arguments = arguments;
        process.Start();
        process.WaitForExit();
        Status = process.ExitCode;
        Error = null;
    }
    catch (System.Exception e) {
        Status = 1;
        Error = e;
    }
").

%---------------------%

:- pragma foreign_proc("Java",
    progname(_Default::in, PrognameOut::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    PrognameOut = jmercury.runtime.JavaInternal.progname;
").

:- pragma foreign_proc("Java",
    command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    String[] arg_vector = jmercury.runtime.JavaInternal.args;
    Args = list.empty_list();
    // arg_vector does not include the executable name.
    for (int i = arg_vector.length - 1; i >= 0; --i) {
        Args = list.cons(arg_vector[i], Args);
    }
").

:- pragma foreign_proc("Java",
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    ExitStatus = jmercury.runtime.JavaInternal.exit_status;
").

:- pragma foreign_proc("Java",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    jmercury.runtime.JavaInternal.exit_status = ExitStatus;
").

:- pragma foreign_proc("Java",
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
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

        Status = process.waitFor();
        Error = null;

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
        Status  = 1;
        Error = e;
    }
").

%---------------------%

:- pragma foreign_proc("Erlang",
    command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    ArgStrings = init:get_plain_arguments(),
    Args = lists:map(fun list_to_binary/1, ArgStrings)
").

:- pragma foreign_proc("Erlang",
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    'ML_erlang_global_server' ! {get_exit_status, self()},
    receive
        {get_exit_status_ack, ExitStatus} ->
            void
    end
").

:- pragma foreign_proc("Erlang",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    'ML_erlang_global_server' ! {set_exit_status, ExitStatus}
").

:- pragma foreign_proc("Erlang",
    call_system_code(Command::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    % XXX this is just a hack
    % 0. assumes unix shell
    % 1. the command cannot receive input
    % 2. output doesn't come out until process finishes
    % 3. the error code is returned in an inefficient way
    % 4. standard output and standard error are always tied together
    % 5. the command should be bracketed/sanitised
    %
    CommandStr = binary_to_list(Command),
    OutputAndCode = os:cmd(CommandStr ++ ""; printf '\\n%d' $?""),
    case string:rchr(OutputAndCode, $\\n) of
        0 ->
            Code = OutputAndCode;
        NL ->
            {Output, [$\\n | Code]} = lists:split(NL - 1, OutputAndCode),
            io:put_chars(Output)
    end,
    {Status, []} = string:to_integer(Code),
    Error = ok
").

%---------------------------------------------------------------------------%
%
% io.getenv and io.setenv.
%

:- pragma foreign_decl("C", "
#include <stdlib.h> /* for getenv() and setenv() */
").

:- pragma foreign_proc("C",
    getenv(Var::in, Value::out),
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
    getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io],
"
    Value = System.Environment.GetEnvironmentVariable(Var);
    SUCCESS_INDICATOR = (Value != null);
").

:- pragma foreign_proc("Java",
    getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io,
        may_not_duplicate],
"
    Value = System.getenv(Var);
    SUCCESS_INDICATOR = (Value != null);
").

:- pragma foreign_proc("Erlang",
    getenv(Var::in, Value::out),
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

:- pragma foreign_proc("C",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, not_thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_WIN32
    SUCCESS_INDICATOR =
        (_wputenv_s(ML_utf8_to_wide(Var), ML_utf8_to_wide(Value)) == 0);
#else
    SUCCESS_INDICATOR = (setenv(Var, Value, 1) == 0);
#endif
").

:- pragma foreign_proc("C#",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io],
"
    try {
        System.Environment.SetEnvironmentVariable(Var, Value);
        SUCCESS_INDICATOR = true;
    } catch (System.Exception) {
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    // Java does not provide a method to set environment variables, only a way
    // to modify the environment when creating a new process.

    // Avoid warning: Var, Value
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Erlang",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io],
"
    VarStr = binary_to_list(Var),
    ValueStr = binary_to_list(Value),
    os:putenv(VarStr, ValueStr),
    SUCCESS_INDICATOR = true
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = MR_TRUE;
").

:- pragma foreign_proc("Java",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("C#",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Erlang",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = true
").

%---------------------------------------------------------------------------%

make_temp_file(Result, !IO) :-
    get_temp_directory(Dir, !IO),
    make_temp_file(Dir, "mtmp", "", Result, !IO).

make_temp(Name, !IO) :-
    make_temp_file(Result, !IO),
    (
        Result = ok(Name)
    ;
        Result = error(Error),
        throw(Error)
    ).

make_temp_file(Dir, Prefix, Suffix, Result, !IO) :-
    do_make_temp(Dir, Prefix, Suffix, char_to_string(dir.directory_separator),
        Name, Error, !IO),
    ( if is_error(Error, "error creating temporary file: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(Name)
    ).

make_temp(Dir, Prefix, Name, !IO) :-
    make_temp_file(Dir, Prefix, "", Result, !IO),
    (
        Result = ok(Name)
    ;
        Result = error(Error),
        throw(Error)
    ).

make_temp_directory(Result, !IO) :-
    get_temp_directory(Dir, !IO),
    make_temp_directory(Dir, "mtmp", "", Result, !IO).

make_temp_directory(Dir, Prefix, Suffix, Result, !IO) :-
    do_make_temp_directory(Dir, Prefix, Suffix,
        char_to_string(dir.directory_separator), DirName, Error, !IO),
    ( if is_error(Error, "error creating temporary directory: ", IOError) then
        Result = error(IOError)
    else
        Result = ok(DirName)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("Java", local,
"
import java.io.File;
import java.io.IOException;
import java.util.Random;
").

:- pragma foreign_code("Java",
"
    public static Random ML_rand = new Random();

    public static String makeTempName(String prefix, String suffix)
    {
        StringBuilder sb = new StringBuilder();

        sb.append(prefix);
        // Make an 8-digit mixed case alpha-numeric code.
        for (int i = 0; i < 8; i++) {
            char c;
            int c_num = ML_rand.nextInt(10+26+26);
            if (c_num < 10) {
                c_num = c_num + '0';
            } else if (c_num < 10+26) {
                c_num = c_num + 'A' - 10;
            } else{
                c_num = c_num + 'a' - 10 - 26;
            }
            c = (char)c_num;
            sb.append(c);
        }
        sb.append(suffix);

        return sb.toString();
    }
").

:- pred do_make_temp(string::in, string::in, string::in, string::in,
    string::out, system_error::out, io::di, io::uo) is det.

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
    do_make_temp(Dir::in, Prefix::in, Suffix::in, Sep::in, FileName::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure,
        not_thread_safe, % due to ML_io_tempnam_counter
        tabled_for_io, does_not_affect_liveness],
"
#ifdef MR_HAVE_MKSTEMP
    int err, fd;

    /*
    ** We cannot append Suffix because the last six chars in the argument
    ** to mkstemp() must be XXXXXX.
    */
    FileName = MR_make_string(MR_ALLOC_ID, ""%s%s%.5sXXXXXX"",
        Dir, Sep, Prefix);
    fd = mkstemp(FileName);
    if (fd == -1) {
        Error = errno;
    } else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == 0) {
            Error = 0;
        } else {
            Error = errno;
        }
    }
#else
    /*
    ** Constructs a temporary name by concatenating Dir, `/', the first 5 chars
    ** of Prefix, six hex digits, and Suffix. The six digit hex number is
    ** generated by starting with the pid of this process.  Uses
    ** `open(..., O_CREATE | O_EXCL, ...)' to create the file, checking that
    ** there was no existing file with that name.
    */
    int     len, err, fd, num_tries;
    char    countstr[256];
    MR_Word filename_word;
    int     flags;

    len = strlen(Dir) + 1 + 5 + 6 + strlen(Suffix) + 1;
    /* Dir + / + Prefix + counter + Suffix + \\0 */
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
        strncat(FileName, countstr, 6);
        strcat(FileName, Suffix);
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
        Error = errno;
    }  else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == 0) {
            Error = 0;
        } else {
            Error = errno;
        }
    }
#endif
").

:- pragma foreign_proc("C#",
    do_make_temp(_Dir::in, _Prefix::in, _Suffix::in, _Sep::in, FileName::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        FileName = System.IO.Path.GetTempFileName();
        Error = null;
    } catch (System.Exception e) {
        FileName = """";
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_make_temp(Dir::in, Prefix::in, Suffix::in, _Sep::in, FileName::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        File    new_file;

        if (Prefix.length() > 5) {
            // The documentation for io.make_temp says that we should only use
            // the first five characters of Prefix.
            Prefix = Prefix.substring(0, 5);
        }

        new_file = new File(new File(Dir), makeTempName(Prefix, Suffix));
        if (new_file.createNewFile()) {
            FileName = new_file.getAbsolutePath();
            Error = null;
        } else {
            FileName = """";
            Error = new java.io.IOException(""Could not create file"");
        }
    } catch (IOException e) {
        FileName = """";
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    do_make_temp(Dir::in, Prefix::in, Suffix::in, Sep::in, FileName::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness],
"
    DirStr = binary_to_list(Dir),
    PrefixStr = binary_to_list(Prefix),
    SuffixStr = binary_to_list(Suffix),
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
        mercury__io:'ML_do_make_temp_2'(DirStr, PrefixStr, SuffixStr, SepStr,
            MaxTries, Seed, eio)
    of
        {ok, FileName0} ->
            FileName = list_to_binary(FileName0),
            Error = ok;
        {error, Reason} ->
            FileName = <<>>,
            Error = {error, Reason}
    end
").

:- pragma foreign_decl("Erlang", local, "
    -export(['ML_do_make_temp_2'/7]).
").
:- pragma foreign_code("Erlang", "
    'ML_do_make_temp_2'(_Dir, _Prefix, _Suffix, _Sep, 0, _Seed, LastError) ->
        {error, LastError};
    'ML_do_make_temp_2'(Dir, Prefix, Suffix, Sep, Tries, Seed0, LastError) ->
        {R1, Seed1} = random:uniform_s(16#1000, Seed0),
        {R2, Seed}  = random:uniform_s(16#1000, Seed1),
        FileName = lists:flatten(io_lib:format(""~s~s~s~3.16.0B.~3.16.0B~s"",
            [Dir, Sep, Prefix, R1, R2, Suffix])),
        case filelib:is_file(FileName) of
            true ->
                'ML_do_make_temp_2'(Dir, Prefix, Suffix, Sep, Tries - 1, Seed,
                    eexist);
            false ->
                case file:open(FileName, [write, {encoding, utf8}]) of
                    {ok, IoDevice} ->
                        case file:close(IoDevice) of
                            ok ->
                                {ok, FileName};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    {error, Reason} ->
                        'ML_do_make_temp_2'(Dir, Prefix, Suffix, Sep,
                            Tries - 1, Seed, Reason)
                end
        end.
").

%-----------------------------------------------------------------------%

:- pred do_make_temp_directory(string::in, string::in, string::in, string::in,
    string::out, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_make_temp_directory(Dir::in, Prefix::in, Suffix::in, Sep::in,
        DirName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness],
"
#ifdef MR_HAVE_MKDTEMP
    int err;

    /*
    ** We cannot append Suffix because the last six chars in the argument
    ** to mkdtemp() must be XXXXXX.
    */
    DirName = MR_make_string(MR_ALLOC_ID, ""%s%s%.5sXXXXXX"",
        Dir, Sep, Prefix);
    DirName = mkdtemp(DirName);
    if (DirName == NULL) {
        Error = errno;
    } else {
        Error = 0;
    }
#else
    Error = ENOSYS;
    DirName = MR_make_string_const("""");
#endif /* HAVE_MKDTEMP */
").

:- pragma foreign_proc("C#",
    do_make_temp_directory(Dir::in, _Prefix::in, _Suffix::in, _Sep::in,
        DirName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    try
    {
        DirName = Path.Combine(Dir, Path.GetRandomFileName());

        switch (Environment.OSVersion.Platform)
        {
            case PlatformID.Win32NT:
                // obtain the owner of the temporary directory
                IdentityReference tempInfo =
                    new DirectoryInfo(Dir)
                        .GetAccessControl(AccessControlSections.Owner)
                        .GetOwner(typeof(SecurityIdentifier));

                DirectorySecurity security = new DirectorySecurity();
                security.AddAccessRule(
                    new FileSystemAccessRule(tempInfo,
                        FileSystemRights.ListDirectory
                            | FileSystemRights.Read
                            | FileSystemRights.Modify,
                        InheritanceFlags.None,
                        PropagationFlags.None,
                        AccessControlType.Allow
                    )
                );
                Directory.CreateDirectory(DirName, security);
                Error = null;
                break;

#if __MonoCS__
            case PlatformID.Unix:
            case (PlatformID)6: // MacOSX:
                int errorNo = ML_sys_mkdir(DirName, 0x7 << 6);
                if (errorNo == 0)
                {
                    Error = null;
                }
                else
                {
                    Error = new System.Exception(string.Format(
                        ""Creating directory {0} failed with: {1:X}"",
                        DirName, errorNo));
                }
                break;
#endif

            default:
                Error = new System.Exception(
                    ""Changing folder permissions is not supported for: "" +
                    Environment.OSVersion);
                break;
        }
    }
    catch (System.Exception e)
    {
        DirName = string.Empty;
        Error = e;
    }
}").

:- pragma foreign_proc("Java",
    do_make_temp_directory(Dir::in, Prefix::in, Suffix::in, _Sep::in,
        DirName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    File    new_dir;

    if (Prefix.length() > 5) {
        // The documentation for io.make_temp says that we should only use
        // the first five characters of Prefix.
        Prefix = Prefix.substring(0, 5);
    }

    new_dir = new File(new File(Dir), makeTempName(Prefix, Suffix));
    if (new_dir.mkdir()) {
        DirName = new_dir.getAbsolutePath();
        Error = null;
    } else {
        DirName = """";
        Error = new java.io.IOException(""Couldn't create directory"");
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HAVE_MKDTEMP
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pragma foreign_proc("Java",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("C#",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Erlang",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false
").

%---------------------------------------------------------------------------%

get_temp_directory(Dir, !IO) :-
    % If using the Java or C# backend then use their API to get the location of
    % temporary files.
    system_temp_dir(Dir0, OK, !IO),
    ( if OK = 1 then
        Dir = Dir0
    else
        % Either this is not a Java or C# grade or the Java or C# backend
        % couldn't determine the temporary directory.
        %
        % We need to do an explicit check of TMPDIR because not all
        % systems check TMPDIR for us (eg Linux #$%*@&).
        Var = ( if dir.use_windows_paths then "TMP" else "TMPDIR" ),
        get_environment_var(Var, Result, !IO),
        (
            Result = yes(Dir)
        ;
            Result = no,
            ( if dir.use_windows_paths then
                Dir = dir.this_directory
            else
                Dir = "/tmp"
            )
        )
    ).

:- pred system_temp_dir(string::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    system_temp_dir(Dir::out, OK::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Dir = java.lang.System.getProperty(""java.io.tmpdir"");
        OK = (Dir != null) ? 1 : 0;
    } catch (Exception e) {
        Dir = null;
        OK = 0;
    }
").

:- pragma foreign_proc("C#",
    system_temp_dir(Dir::out, OK::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Dir = System.IO.Path.GetTempPath();
        OK = (Dir != null) ? 1 : 0;
    } catch (System.Exception _) {
        Dir = null;
        OK = 0;
    }
").

system_temp_dir("", 0, !IO).

%---------------------------------------------------------------------------%

remove_file(FileName, Result, !IO) :-
    remove_file_2(FileName, Error, !IO),
    ( if is_error(Error, "remove failed: ", IOError) then
        Result = error(IOError)
    else
        Result = ok
    ).

:- pred remove_file_2(string::in, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    int rc;
#ifdef MR_WIN32
    rc = _wremove(ML_utf8_to_wide(FileName));
#else
    rc = remove(FileName);
#endif
    if (rc == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
").

:- pragma foreign_proc("C#",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (System.IO.File.Exists(FileName)) {
            System.IO.File.Delete(FileName);
            Error = null;
        } else {
            Error = new System.IO.FileNotFoundException();
        }
    }
    catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // Java 7 java.nio.file.Files.delete() provides more detailed information
    // about failure to delete.

    try {
        java.io.File file = new java.io.File(FileName);

        if (file.delete()) {
            Error = null;
        } else {
            Error = new java.io.IOException(""remove_file failed"");
        }
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    FileNameStr = binary_to_list(FileName),
    case file:delete(FileNameStr) of
        ok ->
            Error = ok;
        {error, Reason} ->
            Error = {error, Reason}
    end
").

%---------------------------------------------------------------------------%

remove_file_recursively(FileName, Res, !IO) :-
    FollowSymLinks = no,
    file_type(FollowSymLinks, FileName, ResFileType, !IO),
    (
        ResFileType = ok(FileType),
        (
            FileType = directory,
            dir.foldl2(remove_directory_entry, FileName, ok, Res0, !IO),
            (
                Res0 = ok(MaybeError),
                (
                    MaybeError = ok,
                    remove_file(FileName, Res, !IO)
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
            remove_file(FileName, Res, !IO)
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

%---------------------------------------------------------------------------%

rename_file(OldFileName, NewFileName, Result, !IO) :-
    rename_file_2(OldFileName, NewFileName, Error, !IO),
    ( if is_error(Error, "rename failed: ", IOError) then
        Result = error(IOError)
    else
        Result = ok
    ).

:- pred rename_file_2(string::in, string::in, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    int rc;
#ifdef MR_WIN32
    rc = _wrename(ML_utf8_to_wide(OldFileName),
        ML_utf8_to_wide(NewFileName));
#else
    rc = rename(OldFileName, NewFileName);
#endif
    if (rc == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
").

:- pragma foreign_proc("C#",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (System.IO.File.Exists(OldFileName)) {
            System.IO.File.Move(OldFileName, NewFileName);
            Error = null;
        } else {
            Error = new System.IO.FileNotFoundException();
        }
    }
    catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // Java 7 java.nio.file.Files.move may provide more detailed information
    // about failure to rename.

    try {
        java.io.File file = new java.io.File(OldFileName);

        if (file.exists()) {
            if (file.renameTo(new java.io.File(NewFileName))) {
                Error = null;
            } else {
                Error = new java.io.IOException(""rename_file failed"");
            }
        } else {
            Error = new java.io.IOException(""No such file or directory"");
        }
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    OldFileNameStr = binary_to_list(OldFileName),
    NewFileNameStr = binary_to_list(NewFileName),
    case file:rename(OldFileNameStr, NewFileNameStr) of
        ok ->
            Error = ok;
        {error, Reason} ->
            Error = {error, Reason}
    end
").

%---------------------------------------------------------------------------%

have_symlinks :- semidet_fail.

:- pragma foreign_proc("C",
    have_symlinks,
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
    have_symlinks,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    % XXX how do we check without actually trying to make one?
    SUCCESS_INDICATOR = true
").

make_symlink(FileName, LinkFileName, Result, !IO) :-
    ( if io.have_symlinks then
        io.make_symlink_2(FileName, LinkFileName, Error, !IO),
        ( if is_error(Error, "io.make_symlink failed: ", IOError) then
            Result = error(IOError)
        else
            Result = ok
        )
    else
        Result = error(make_io_error(
            "io.make_symlink not supported on this platform"))
    ).

:- pred make_symlink_2(string::in, string::in, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_symlink_2(FileName::in, LinkFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_SYMLINK
    if (symlink(FileName, LinkFileName) == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
#else
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("Erlang",
    make_symlink_2(FileName::in, LinkFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    FileNameStr = binary_to_list(FileName),
    LinkFileNameStr = binary_to_list(LinkFileName),
    case file:make_symlink(FileNameStr, LinkFileNameStr) of
        ok ->
            Error = ok;
        {error, Reason} ->
            Error = {error, Reason}
    end
").

read_symlink(FileName, Result, !IO) :-
    ( if have_symlinks then
        read_symlink_2(FileName, TargetFileName, Error, !IO),
        ( if is_error(Error, "io.read_symlink failed: ", IOError) then
            Result = error(IOError)
        else
            Result = ok(TargetFileName)
        )
    else
        Result = error(make_io_error(
            "io.read_symlink not supported on this platform"))
    ).

:- pred read_symlink_2(string::in, string::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_symlink_2(FileName::in, TargetFileName::out, Error::out,
        _IO0::di, _IO::uo),
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
            num_chars = readlink(FileName, buffer2, buffer_size2);
        } while (num_chars == buffer_size2);

        /* Invariant: num_chars < buffer_size2 */

        if (num_chars == -1) {
            TargetFileName = MR_make_string_const("""");
            Error = errno;
        } else {
            buffer2[num_chars] = '\\0';
            MR_make_aligned_string_copy_msg(TargetFileName, buffer2,
                MR_ALLOC_ID);
            Error = 0;
        }
        MR_free(buffer2);
    } else if (num_chars == -1) {
        TargetFileName = MR_make_string_const("""");
        Error = errno;
    } else {
        buffer[num_chars] = '\\0';
        MR_make_aligned_string_copy_msg(TargetFileName, buffer, MR_ALLOC_ID);
        Error = 0;
    }
#else /* !MR_HAVE_READLINK */
    TargetFileName = MR_make_string_const("""");
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("Erlang",
    read_symlink_2(FileName::in, TargetFileName::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    case file:read_link(binary_to_list(FileName)) of
        {ok, TargetFileNameStr} ->
            TargetFileName = list_to_binary(TargetFileNameStr),
            Error = ok;
        {error, Reason} ->
            TargetFileName = <<>>,
            Error = {error, Reason}
    end
").

% Since io.have_symlinks will fail for Java, these procedures should never be
% called:
% XXX Java 7 has createSymbolicLink, readSymbolicLink

:- pragma foreign_proc("Java",
    make_symlink_2(_FileName::in, _LinkFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    Error = new java.lang.UnsupportedOperationException(
        ""io.make_symlink_2 not implemented"");
").

:- pragma foreign_proc("Java",
    read_symlink_2(_FileName::in, TargetFileName::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    TargetFileName = """";
    Error = new java.lang.UnsupportedOperationException(
        ""io.read_symlink_2 not implemented"");
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Instances of the stream typeclass.
%

:- instance stream.error(error) where [
    func(stream.error_message/1) is io.error_message
].

%---------------------------------------------------------------------------%
%
% Text input streams.
%

:- instance stream.stream(input_stream, io) where [
    pred(name/4) is input_stream_name
].

:- instance stream.input(input_stream, io) where [].

:- instance stream.reader(input_stream, char, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_char(Stream, Result0, !IO),
        Result = io.result_to_stream_result(Result0)
    )
].

:- instance stream.reader(input_stream, line, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_line_as_string(Stream, Result0, !IO),
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

:- instance stream.reader(input_stream, text_file, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_file_as_string(Stream, Result0, !IO),
        (
            Result0 = ok(String),
            Result = ok(text_file(String))
        ;
            Result0 = error(_PartialString, Error),
            Result = error(Error)
        )
    )
].

:- instance stream.putback(input_stream, char, io, io.error) where
[
    pred(unget/4) is putback_char
].

:- func result_to_stream_result(io.result(T)) = stream.result(T, io.error).

result_to_stream_result(ok(T)) = ok(T).
result_to_stream_result(eof) = eof.
result_to_stream_result(error(Error)) = error(Error).

:- instance stream.line_oriented(input_stream, io) where
[
    pred(get_line/4) is io.get_line_number,
    pred(set_line/4) is io.set_line_number
].

%---------------------------------------------------------------------------%
%
% Text output streams.
%

:- instance stream.stream(output_stream, io) where [
    pred(name/4) is output_stream_name
].

:- instance stream.output(output_stream, io) where [
    pred(flush/3) is flush_output
].

:- instance stream.writer(output_stream, char, io)
    where
[
    pred(put/4) is write_char
].

:- instance stream.writer(output_stream, float, io)
    where
[
    pred(put/4) is write_float
].

:- instance stream.writer(output_stream, int, io)
    where
[
    pred(put/4) is write_int
].

:- instance stream.writer(output_stream, int8, io)
    where
[
    pred(put/4) is write_int8
].

:- instance stream.writer(output_stream, int16, io)
    where
[
    pred(put/4) is write_int16
].

:- instance stream.writer(output_stream, int32, io)
    where
[
    pred(put/4) is write_int32
].

:- instance stream.writer(output_stream, uint, io)
    where
[
    pred(put/4) is write_uint
].

:- instance stream.writer(output_stream, uint8, io)
    where
[
    pred(put/4) is write_uint8
].

:- instance stream.writer(output_stream, uint16, io)
    where
[
    pred(put/4) is write_uint16
].

:- instance stream.writer(output_stream, uint32, io)
    where
[
    pred(put/4) is write_uint32
].

:- instance stream.writer(output_stream, string, io)
    where
[
    pred(put/4) is write_string
].

:- instance stream.writer(output_stream, univ, io)
    where
[
    pred(put/4) is stream.string_writer.write_univ
].

:- instance stream.line_oriented(output_stream, io) where
[
    pred(get_line/4) is get_output_line_number,
    pred(set_line/4) is set_output_line_number
].

%---------------------------------------------------------------------------%
%
% Binary input streams.
%

:- instance stream.stream(binary_input_stream, io)
    where
[
    pred(name/4) is binary_input_stream_name
].

:- instance stream.input(binary_input_stream, io)
    where [].

:- instance stream.reader(binary_input_stream, int, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_byte(Stream, Result0, !IO),
        Result = result_to_stream_result(Result0)
    )
].

:- instance stream.bulk_reader(binary_input_stream, int,
        bitmap, io, io.error)
    where
[
    ( bulk_get(Stream, Index, Int, !Store, NumRead, Result, !State) :-
        read_bitmap(Stream, Index, Int, !Store, NumRead,
            Result0, !State),
        Result = res_to_stream_res(Result0)
    )
].

:- instance stream.putback(binary_input_stream, int, io, io.error)
    where
[
    pred(unget/4) is putback_byte
].

:- instance stream.seekable(binary_input_stream, io)
    where
[
    ( seek(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        seek_binary_input(Stream, Whence, OffSet, !IO)
    )
].

:- func stream_whence_to_io_whence(stream.whence) = io.whence.

stream_whence_to_io_whence(set) = set.
stream_whence_to_io_whence(cur) = cur.
stream_whence_to_io_whence(end) = end.

:- func io.res_to_stream_res(io.res) = stream.res(io.error).

res_to_stream_res(ok) = ok.
res_to_stream_res(error(E)) = error(E).

%---------------------------------------------------------------------------%
%
% Binary output streams.
%

:- instance stream.stream(binary_output_stream, io)
    where
[
    pred(name/4) is binary_output_stream_name
].

:- instance stream.output(binary_output_stream, io)
    where
[
    pred(flush/3) is flush_binary_output
].

:- instance stream.writer(binary_output_stream, byte, io)
    where
[
    pred(put/4) is write_byte
].

:- instance stream.writer(binary_output_stream, int8, io)
    where
[
    pred(put/4) is write_binary_int8
].

:- instance stream.writer(binary_output_stream, uint8, io)
    where
[
    pred(put/4) is write_binary_uint8
].

:- instance stream.writer(binary_output_stream, bitmap, io)
    where
[
    pred(put/4) is write_bitmap
].

:- instance stream.writer(binary_output_stream, bitmap.slice, io)
    where
[
    ( put(Stream, Slice, !IO) :-
        write_bitmap(Stream, Slice ^ slice_bitmap,
            Slice ^ slice_start_byte_index, Slice ^ slice_num_bytes, !IO)
    )
].

:- instance stream.seekable(binary_output_stream, io)
    where
[
    ( seek(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        seek_binary_output(Stream, Whence, OffSet, !IO)
    )
].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
