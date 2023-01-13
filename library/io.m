%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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
% also ensure that the program doesn't attempt to backtrack over any I/O.
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

:- include_module call_system.
:- include_module environment.
:- include_module file.

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

:- type maybe_partial_res_2(T1, T2)
    --->    ok2(T1, T2)
    ;       error2(T1, T2, io.error).

:- inst maybe_partial_res(T) for maybe_partial_res/1
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

    % maybe_incomplete_result is returned when reading multibyte values from a
    % binary stream. `incomplete(Bytes)' is returned when at least one byte of
    % a value has already been read but there are insufficient bytes
    % remaining the stream to complete the value. In that case, Bytes will
    % contain the bytes that have already been read from the stream, in the
    % order in which they were read.
    %
:- type maybe_incomplete_result(T)
    --->    ok(T)
    ;       eof
    ;       incomplete(list(uint8))
    ;       error(io.error).

:- type read_result(T)
    --->    ok(T)
    ;       eof
    ;       error(string, int). % error message, line number

    % A value indicating an error.
    % This may or may not have an associated io.system_error value.
    %
:- type io.error.

    % A system-dependent error value.
    %
    % For C backends, this is either an errno value (e.g. ENOENT)
    % or a Windows system error code (e.g. ERROR_FILE_NOT_FOUND).
    % A value of 0 represents success in both cases.
    %
    % For the Java and C# backends, this is an exception object or null,
    % where null represents no error.
    %
:- type system_error.
:- pragma foreign_type(c, system_error, "MR_Integer",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", system_error, "System.Exception").
:- pragma foreign_type(java, system_error, "java.lang.Exception").

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
% Opening and closing streams, both text and binary.
%

    % Attempts to open a text file for input.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_input(string::in, io.res(io.text_input_stream)::out,
    io::di, io::uo) is det.

    % Attempts to open a binary file for input.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_binary_input(string::in,
    io.res(io.binary_input_stream)::out, io::di, io::uo) is det.

%---------------------%

    % Attempts to open a text file for output.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_output(string::in, io.res(io.text_output_stream)::out,
    io::di, io::uo) is det.

    % Attempts to open a file for binary output.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_binary_output(string::in,
    io.res(io.binary_output_stream)::out, io::di, io::uo) is det.

%---------------------%

    % Attempts to open a text file for appending.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_append(string::in, io.res(io.text_output_stream)::out,
    io::di, io::uo) is det.

    % Attempts to open a file for binary appending.
    % Result is either 'ok(Stream)' or 'error(ErrorCode)'.
    %
:- pred open_binary_append(string::in,
    io.res(io.binary_output_stream)::out, io::di, io::uo) is det.

%---------------------%

    % Closes an open text input stream.
    % Throw an io.error exception if an I/O error occurs.
    %
:- pred close_input(io.text_input_stream::in, io::di, io::uo) is det.

    % Closes an open binary input stream. This will throw an io.error
    % exception if an I/O error occurs.
    %
:- pred close_binary_input(io.binary_input_stream::in,
    io::di, io::uo) is det.

%---------------------%

    % Closes an open text output stream.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred close_output(io.text_output_stream::in, io::di, io::uo) is det.

    % Closes an open binary output stream.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred close_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Switching streams.
%

    % set_input_stream(NewStream, OldStream, !IO):
    % Changes the current input stream to NewStream.
    % Returns the previous input stream as OldStream.
    %
:- pred set_input_stream(io.text_input_stream::in,
    io.text_input_stream::out, io::di, io::uo) is det.

    % Changes the current input stream to the stream specified.
    % Returns the previous stream.
    %
:- pred set_binary_input_stream(io.binary_input_stream::in,
    io.binary_input_stream::out, io::di, io::uo) is det.

%---------------------%

    % set_output_stream(NewStream, OldStream, !IO):
    % Changes the current output stream to NewStream.
    % Returns the previous output stream as OldStream.
    %
:- pred set_output_stream(io.text_output_stream::in,
    io.text_output_stream::out, io::di, io::uo) is det.

    % Changes the current binary output stream to the stream specified.
    % Returns the previous stream.
    %
:- pred set_binary_output_stream(io.binary_output_stream::in,
    io.binary_output_stream::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Seeking on binary streams.
%

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary input stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
    % A successful seek undoes any effects of putback_byte on the stream.
    %
:- pred seek_binary_input(io.binary_input_stream::in, io.whence::in,
    int::in, io::di, io::uo) is det.

    % As above, but the offset is always a 64-bit value.
    %
:- pred seek_binary_input64(io.binary_input_stream::in, io.whence::in,
    int64::in, io::di, io::uo) is det.

%---------------------%

    % Seek to an offset relative to Whence (documented above)
    % on a specified binary output stream. Attempting to seek on a pipe
    % or tty results in implementation dependent behaviour.
    %
:- pred seek_binary_output(io.binary_output_stream::in, io.whence::in,
    int::in, io::di, io::uo) is det.

    % As above, but the offset is always a 64-bit value.
    %
:- pred seek_binary_output64(io.binary_output_stream::in, io.whence::in,
    int64::in, io::di, io::uo) is det.

%---------------------%

    % Returns the offset (in bytes) into the specified binary input stream.
    % Throws an exception if the offset is outside the range that can be
    % represented by the int type. To avoid this possibility, you can use the
    % 64-bit offset version of this predicate below.
    %
:- pred binary_input_stream_offset(io.binary_input_stream::in, int::out,
    io::di, io::uo) is det.

    % As above, but the offset is always a 64-bit value.
    %
:- pred binary_input_stream_offset64(io.binary_input_stream::in, int64::out,
    io::di, io::uo) is det.

%---------------------%

    % Returns the offset (in bytes) into the specified binary output stream.
    % Throws an exception if the offset is outside the range that can be
    % represented by the int type. To avoid this possibility, you can use the
    % 64-bit offset version of this predicate below.
    %
:- pred binary_output_stream_offset(io.binary_output_stream::in, int::out,
    io::di, io::uo) is det.

    % As above, but the offset is always a 64-bit value.
    %
:- pred binary_output_stream_offset64(io.binary_output_stream::in, int64::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Standard stream id predicates.
%

    % Retrieves the standard input stream.
    %
:- func stdin_stream = io.text_input_stream.

    % Retrieves the standard input stream.
    % Does not modify the I/O state.
    %
:- pred stdin_stream(io.text_input_stream::out, io::di, io::uo) is det.

    % Retrieves the standard binary input stream.
    % Does not modify the I/O state.
    %
:- pred stdin_binary_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

%---------------------%

    % Retrieves the standard output stream.
    %
:- func stdout_stream = io.text_output_stream.

    % Retrieves the standard output stream.
    % Does not modify the I/O state.
    %
:- pred stdout_stream(io.text_output_stream::out, io::di, io::uo) is det.

    % Retrieves the standard binary output stream.
    % Does not modify the I/O state.
    %
:- pred stdout_binary_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

%---------------------%

    % Retrieves the standard error stream.
    %
:- func stderr_stream = io.text_output_stream.

    % Retrieves the standard error stream.
    % Does not modify the I/O state.
    %
:- pred stderr_stream(io.text_output_stream::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Current stream id predicates.
%

    % Retrieves the current input stream.
    % Does not modify the I/O state.
    %
:- pred input_stream(io.text_input_stream::out, io::di, io::uo) is det.

    % Retrieves the current binary input stream.
    % Does not modify the I/O state.
    %
:- pred binary_input_stream(io.binary_input_stream::out,
    io::di, io::uo) is det.

%---------------------%

    % Retrieves the current output stream.
    % Does not modify the I/O state.
    %
:- pred output_stream(io.text_output_stream::out, io::di, io::uo) is det.

    % Retrieves the current binary output stream.
    % Does not modify the I/O state.
    %
:- pred binary_output_stream(io.binary_output_stream::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Getting and setting stream properties.
%

    % Retrieves the human-readable name associated with the current input
    % stream or the specified output stream. For file streams, this is
    % the filename. For stdin, this is the string "<standard input>".
    %
:- pred input_stream_name(string::out, io::di, io::uo) is det.
:- pred input_stream_name(io.text_input_stream::in, string::out,
    io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current binary
    % input stream or the specified binary input stream. For file streams,
    % this is the filename.
    %
:- pred binary_input_stream_name(string::out, io::di, io::uo) is det.
:- pred binary_input_stream_name(io.binary_input_stream::in, string::out,
    io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current
    % output stream or the specified output stream.
    % For file streams, this is the filename.
    % For stdout this is the string "<standard output>".
    % For stderr this is the string "<standard error>".
    %
:- pred output_stream_name(string::out, io::di, io::uo) is det.
:- pred output_stream_name(io.text_output_stream::in, string::out,
    io::di, io::uo) is det.

    % Retrieves the human-readable name associated with the current
    % binary output stream or the specified binary output stream.
    % For file streams, this is the filename.
    %
:- pred binary_output_stream_name(string::out, io::di, io::uo) is det.
:- pred binary_output_stream_name(io.binary_output_stream::in,
    string::out, io::di, io::uo) is det.

%---------------------%

    % Return the line number of the current input stream or the specified
    % input stream. Lines are normally numbered starting at 1, but this
    % can be overridden by calling set_line_number.
    %
:- pred get_line_number(int::out, io::di, io::uo) is det.
:- pred get_line_number(io.text_input_stream::in, int::out, io::di, io::uo)
    is det.

    % Set the line number of the current input stream or the specified
    % input stream.
    %
:- pred set_line_number(int::in, io::di, io::uo) is det.
:- pred set_line_number(io.text_input_stream::in, int::in, io::di, io::uo)
    is det.

    % Return the line number of the current output stream or the
    % specified output stream. Lines are normally numbered starting at 1,
    % but this can be overridden by calling set_output_line_number.
    %
:- pred get_output_line_number(int::out, io::di, io::uo) is det.
:- pred get_output_line_number(io.text_output_stream::in, int::out,
    io::di, io::uo) is det.

    % Set the line number of the current output stream.
    %
:- pred set_output_line_number(int::in, io::di, io::uo) is det.
:- pred set_output_line_number(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Reading values of primitive types.
%

    % Read a character (code point) from the current input stream
    % or from the specified stream.
    %
:- pred read_char(io.result(char)::out, io::di, io::uo) is det.
:- pred read_char(io.text_input_stream::in, io.result(char)::out,
    io::di, io::uo) is det.

    % Reads a character (code point) from the specified stream.
    % This interface avoids memory allocation when there is no error.
    %
:- pred read_char_unboxed(io.text_input_stream::in, io.result::out, char::out,
    io::di, io::uo) is det.
% NOTE_TO_IMPLEMENTORS There is no version without an explicit stream argument.

    % Un-read a character (code point) from the current input stream
    % or from the specified stream.
    % You can put back as many characters as you like.
    % You can even put back something that you didn't actually read.
    %
    % On some systems and backends, only one byte of pushback is guaranteed.
    % putback_char will throw an io.error exception if the pushback buffer
    % is full.
    %
:- pred putback_char(char::in, io::di, io::uo) is det.
:- pred putback_char(io.text_input_stream::in, char::in, io::di, io::uo)
    is det.

% Note that there are no read equivalents of write_int, write_intN,
% write_uint, write_uintN, or write_float. Mercury programs that want to read
% numbers must first read in strings, and try to convert the appropriate
% parts of those strings to numbers. This allows them to handle any errors
% in that conversion process in whatever way they like. Since there are many
% possible ways to handle conversion failures, it is not very likely that
% a programmer's chosen method would agree with the one used by a
% system-supplied predicate for reading in e.g. floats, if this module had one.

%---------------------%

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

    % Reads a single signed 8-bit integer from the specified binary input
    % stream. This interface avoids memory allocation when there is no error.
    %
:- pred read_binary_int8_unboxed(io.binary_input_stream::in, io.result::out,
    int8::out, io::di, io::uo) is det.
% NOTE_TO_IMPLEMENTORS There is no version without an explicit stream argument.

    % Reads a single unsigned 8-bit integer from the current binary input
    % stream or from the specified binary input stream.
    %
:- pred read_binary_uint8(io.result(uint8)::out, io::di, io::uo) is det.
:- pred read_binary_uint8(io.binary_input_stream::in, io.result(uint8)::out,
    io::di, io::uo) is det.

    % Reads a single unsigned 8-bit integer from the specified binary input
    % stream. This interface avoids memory allocation when there is no error.
    %
:- pred read_binary_uint8_unboxed(io.binary_input_stream::in, io.result::out,
    uint8::out, io::di, io::uo) is det.
% NOTE_TO_IMPLEMENTORS There is no version without an explicit stream argument.

%---------------------%

    % Un-reads a byte from the current binary input stream or from the
    % specified stream. The byte is taken from the bottom 8 bits of the
    % specified int.
    %
    % You can put back as many bytes as you like.
    % You can even put back something that you did not actually read.
    %
    % On some systems and backends, only one byte of pushback is guaranteed.
    % putback_byte will throw an io.error exception if the pushback buffer
    % is full.
    %
    % Pushing back a byte decrements the file position by one, except when
    % the file position is already zero, in which case the new file position
    % is unspecified.
    %
:- pred putback_byte(int::in, io::di, io::uo) is det.
:- pred putback_byte(io.binary_input_stream::in, int::in, io::di, io::uo)
    is det.

    % Like putback_byte, but where the byte value un-read is the 8 bits of the
    % int8 reinterpreted as a uint8.
    %
:- pred putback_int8(int8::in, io::di, io::uo) is det.
:- pred putback_int8(io.binary_input_stream::in, int8::in, io::di, io::uo)
    is det.

    % Like putback_byte, but where the byte value un-read is the 8 bits of the
    % uint8.
    %
:- pred putback_uint8(uint8::in, io::di, io::uo) is det.
:- pred putback_uint8(io.binary_input_stream::in, uint8::in, io::di, io::uo)
    is det.

%---------------------%

    % The following predicates read multibyte integer values, either from
    % the current binary input stream, or from the specified binary
    % input stream.
    %
    % The names of these predicates have the form:
    %
    %    read_binary_<TYPE><SUFFIX>
    %
    % where <TYPE> is the name of one of the Mercury multibyte fixed size
    % integer types. The optional <SUFFIX> specifies the order in which
    % the bytes that make up the multibyte integer occur in input stream.
    % The suffix may be one of:
    %
    % "_le":    the bytes are in little endian byte order.
    % "_be":    the bytes are in big endian byte order.
    % none:     the bytes are in the byte order of the underlying platform.

:- pred read_binary_int16(maybe_incomplete_result(int16)::out,
    io::di, io::uo) is det.
:- pred read_binary_int16(io.binary_input_stream::in,
    maybe_incomplete_result(int16)::out, io::di, io::uo) is det.
:- pred read_binary_int16_le(maybe_incomplete_result(int16)::out,
    io::di, io::uo) is det.
:- pred read_binary_int16_le(io.binary_input_stream::in,
    maybe_incomplete_result(int16)::out, io::di, io::uo) is det.
:- pred read_binary_int16_be(maybe_incomplete_result(int16)::out,
    io::di, io::uo) is det.
:- pred read_binary_int16_be(io.binary_input_stream::in,
    maybe_incomplete_result(int16)::out, io::di, io::uo) is det.
:- pred read_binary_uint16(maybe_incomplete_result(uint16)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint16(io.binary_input_stream::in,
    maybe_incomplete_result(uint16)::out, io::di, io::uo) is det.
:- pred read_binary_uint16_le(maybe_incomplete_result(uint16)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint16_le(io.binary_input_stream::in,
    maybe_incomplete_result(uint16)::out, io::di, io::uo) is det.
:- pred read_binary_uint16_be(maybe_incomplete_result(uint16)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint16_be(io.binary_input_stream::in,
    maybe_incomplete_result(uint16)::out, io::di, io::uo) is det.

:- pred read_binary_int32(maybe_incomplete_result(int32)::out,
    io::di, io::uo) is det.
:- pred read_binary_int32(io.binary_input_stream::in,
    maybe_incomplete_result(int32)::out, io::di, io::uo) is det.
:- pred read_binary_int32_le(maybe_incomplete_result(int32)::out,
    io::di, io::uo) is det.
:- pred read_binary_int32_le(io.binary_input_stream::in,
    maybe_incomplete_result(int32)::out, io::di, io::uo) is det.
:- pred read_binary_int32_be(maybe_incomplete_result(int32)::out,
    io::di, io::uo) is det.
:- pred read_binary_int32_be(io.binary_input_stream::in,
    maybe_incomplete_result(int32)::out, io::di, io::uo) is det.
:- pred read_binary_uint32(maybe_incomplete_result(uint32)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint32(io.binary_input_stream::in,
    maybe_incomplete_result(uint32)::out, io::di, io::uo) is det.
:- pred read_binary_uint32_le(maybe_incomplete_result(uint32)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint32_le(io.binary_input_stream::in,
    maybe_incomplete_result(uint32)::out, io::di, io::uo) is det.
:- pred read_binary_uint32_be(maybe_incomplete_result(uint32)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint32_be(io.binary_input_stream::in,
    maybe_incomplete_result(uint32)::out, io::di, io::uo) is det.

:- pred read_binary_int64(maybe_incomplete_result(int64)::out,
    io::di, io::uo) is det.
:- pred read_binary_int64(io.binary_input_stream::in,
    maybe_incomplete_result(int64)::out, io::di, io::uo) is det.
:- pred read_binary_int64_le(maybe_incomplete_result(int64)::out,
    io::di, io::uo) is det.
:- pred read_binary_int64_le(io.binary_input_stream::in,
    maybe_incomplete_result(int64)::out, io::di, io::uo) is det.
:- pred read_binary_int64_be(maybe_incomplete_result(int64)::out,
    io::di, io::uo) is det.
:- pred read_binary_int64_be(io.binary_input_stream::in,
    maybe_incomplete_result(int64)::out, io::di, io::uo) is det.
:- pred read_binary_uint64(maybe_incomplete_result(uint64)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint64(io.binary_input_stream::in,
    maybe_incomplete_result(uint64)::out, io::di, io::uo) is det.
:- pred read_binary_uint64_le(maybe_incomplete_result(uint64)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint64_le(io.binary_input_stream::in,
    maybe_incomplete_result(uint64)::out, io::di, io::uo) is det.
:- pred read_binary_uint64_be(maybe_incomplete_result(uint64)::out,
    io::di, io::uo) is det.
:- pred read_binary_uint64_be(io.binary_input_stream::in,
    maybe_incomplete_result(uint64)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Writing values of primitive types.
%

    % Writes a character to the current output stream
    % or to the specified output stream.
    %
:- pred write_char(char::in, io::di, io::uo) is det.
:- pred write_char(io.text_output_stream::in, char::in, io::di, io::uo)
    is det.

    % Writes a signed or unsigned integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int(int::in, io::di, io::uo) is det.
:- pred write_int(io.text_output_stream::in, int::in, io::di, io::uo) is det.
:- pred write_uint(uint::in, io::di, io::uo) is det.
:- pred write_uint(io.text_output_stream::in, uint::in, io::di, io::uo) is det.

    % Write a signed or unsigned 8-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int8(int8::in, io::di, io::uo) is det.
:- pred write_int8(io.text_output_stream::in, int8::in, io::di, io::uo) is det.
:- pred write_uint8(uint8::in, io::di, io::uo) is det.
:- pred write_uint8(io.text_output_stream::in, uint8::in, io::di, io::uo)
    is det.

    % Write a signed or unsigned 16-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int16(int16::in, io::di, io::uo) is det.
:- pred write_int16(io.text_output_stream::in, int16::in, io::di, io::uo)
    is det.
:- pred write_uint16(uint16::in, io::di, io::uo) is det.
:- pred write_uint16(io.text_output_stream::in, uint16::in, io::di, io::uo)
    is det.

    % Write a signed or unsigned 32-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int32(int32::in, io::di, io::uo) is det.
:- pred write_int32(io.text_output_stream::in, int32::in, io::di, io::uo)
    is det.
:- pred write_uint32(uint32::in, io::di, io::uo) is det.
:- pred write_uint32(io.text_output_stream::in, uint32::in, io::di, io::uo)
    is det.

    % Write a signed or unsigned 64-bit integer to the current output stream
    % or to the specified output stream.
    %
:- pred write_int64(int64::in, io::di, io::uo) is det.
:- pred write_int64(io.text_output_stream::in, int64::in, io::di, io::uo)
    is det.
:- pred write_uint64(uint64::in, io::di, io::uo) is det.
:- pred write_uint64(io.text_output_stream::in, uint64::in, io::di, io::uo)
    is det.

    % Writes a floating point number to the current output stream
    % or to the specified output stream.
    %
:- pred write_float(float::in, io::di, io::uo) is det.
:- pred write_float(io.text_output_stream::in, float::in, io::di, io::uo)
    is det.

    % Writes a string to the current output stream or to the
    % specified output stream.
    %
:- pred write_string(string::in, io::di, io::uo) is det.
:- pred write_string(io.text_output_stream::in, string::in, io::di, io::uo)
    is det.

    % Writes a newline character to the current output stream
    % or to the specified stream.
    %
:- pred nl(io::di, io::uo) is det.
:- pred nl(io.text_output_stream::in, io::di, io::uo) is det.

%---------------------%

    % Writes a single byte to the current binary output stream
    % or to the specified binary output stream. The byte is taken from
    % the bottom 8 bits of the specified int.
    %
:- pred write_byte(int::in, io::di, io::uo) is det.
:- pred write_byte(io.binary_output_stream::in, int::in, io::di, io::uo)
    is det.

    % Writes a signed or unsigned 8-bit integer to the current binary
    % output stream or to the specified binary output stream.
    %
:- pred write_binary_int8(int8::in, io::di, io::uo) is det.
:- pred write_binary_int8(io.binary_output_stream::in, int8::in,
    io::di, io::uo) is det.
:- pred write_binary_uint8(uint8::in, io::di, io::uo) is det.
:- pred write_binary_uint8(io.binary_output_stream::in, uint8::in,
    io::di, io::uo) is det.

%---------------------%

    % The following predicates write multibyte integer values, either to the
    % current binary output stream, or to the specified binary output stream.
    %
    % These names of these predicates have the form:
    %
    %    write_binary_<TYPE><SUFFIX>
    %
    % where <TYPE> is the name of one of the Mercury multibyte fixed size
    % integer types. The optional <SUFFIX> specifies the order in which
    % the bytes that make up the multibyte integer are written to the stream.
    % The suffix may be one of:
    %
    % "_le":    the bytes are in little endian byte order.
    % "_be":    the bytes are in big endian byte order.
    % none:     the bytes are in the byte order of the underlying platform.

:- pred write_binary_int16(int16::in, io::di, io::uo) is det.
:- pred write_binary_int16(io.binary_output_stream::in, int16::in,
    io::di, io::uo) is det.
:- pred write_binary_uint16(uint16::in, io::di, io::uo) is det.
:- pred write_binary_uint16(io.binary_output_stream::in, uint16::in,
    io::di, io::uo) is det.
:- pred write_binary_int16_le(int16::in, io::di, io::uo) is det.
:- pred write_binary_int16_le(io.binary_output_stream::in, int16::in,
    io::di, io::uo) is det.
:- pred write_binary_uint16_le(uint16::in, io::di, io::uo) is det.
:- pred write_binary_uint16_le(io.binary_output_stream::in, uint16::in,
    io::di, io::uo) is det.
:- pred write_binary_int16_be(int16::in, io::di, io::uo) is det.
:- pred write_binary_int16_be(io.binary_output_stream::in, int16::in,
    io::di, io::uo) is det.
:- pred write_binary_uint16_be(uint16::in, io::di, io::uo) is det.
:- pred write_binary_uint16_be(io.binary_output_stream::in, uint16::in,
    io::di, io::uo) is det.

:- pred write_binary_int32(int32::in, io::di, io::uo) is det.
:- pred write_binary_int32(io.binary_output_stream::in, int32::in,
    io::di, io::uo) is det.
:- pred write_binary_uint32(uint32::in, io::di, io::uo) is det.
:- pred write_binary_uint32(io.binary_output_stream::in, uint32::in,
    io::di, io::uo) is det.
:- pred write_binary_int32_le(int32::in, io::di, io::uo) is det.
:- pred write_binary_int32_le(io.binary_output_stream::in, int32::in,
    io::di, io::uo) is det.
:- pred write_binary_uint32_le(uint32::in, io::di, io::uo) is det.
:- pred write_binary_uint32_le(io.binary_output_stream::in, uint32::in,
    io::di, io::uo) is det.
:- pred write_binary_int32_be(int32::in, io::di, io::uo) is det.
:- pred write_binary_int32_be(io.binary_output_stream::in, int32::in,
    io::di, io::uo) is det.
:- pred write_binary_uint32_be(uint32::in, io::di, io::uo) is det.
:- pred write_binary_uint32_be(io.binary_output_stream::in, uint32::in,
    io::di, io::uo) is det.

:- pred write_binary_int64(int64::in, io::di, io::uo) is det.
:- pred write_binary_int64(io.binary_output_stream::in, int64::in,
    io::di, io::uo) is det.
:- pred write_binary_uint64(uint64::in, io::di, io::uo) is det.
:- pred write_binary_uint64(io.binary_output_stream::in, uint64::in,
    io::di, io::uo) is det.
:- pred write_binary_int64_le(int64::in, io::di, io::uo) is det.
:- pred write_binary_int64_le(io.binary_output_stream::in, int64::in,
    io::di, io::uo) is det.
:- pred write_binary_uint64_le(uint64::in, io::di, io::uo) is det.
:- pred write_binary_uint64_le(io.binary_output_stream::in, uint64::in,
    io::di, io::uo) is det.
:- pred write_binary_int64_be(int64::in, io::di, io::uo) is det.
:- pred write_binary_int64_be(io.binary_output_stream::in, int64::in,
    io::di, io::uo) is det.
:- pred write_binary_uint64_be(uint64::in, io::di, io::uo) is det.
:- pred write_binary_uint64_be(io.binary_output_stream::in, uint64::in,
    io::di, io::uo) is det.

%---------------------%

    % Write the UTF-8 encoding of a string to the current binary output stream
    % or the specified binary output stream. If the given string is not
    % well-formed, then the behaviour is implementation dependent.
    %
:- pred write_binary_string_utf8(string::in, io::di, io::uo) is det.
:- pred write_binary_string_utf8(io.binary_output_stream::in, string::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Text input predicates.
%

    % Read a whitespace delimited word from the current input stream
    % or from the specified stream.
    %
    % See char.is_whitespace for the definition of whitespace characters
    % used by this predicate.
    %
:- pred read_word(io.result(list(char))::out, io::di, io::uo) is det.
:- pred read_word(io.text_input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Read a line from the current input stream or from the specified
    % stream, returning the result as a list of characters (code points).
    %
    % See the documentation for string.line for the definition of a line.
    %
:- pred read_line(io.result(list(char))::out, io::di, io::uo) is det.
:- pred read_line(io.text_input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

    % Read a line from the current input stream or from the specified
    % stream, returning the result as a string.
    %
    % See the documentation for string.line for the definition of a line.
    %
    % WARNING: the returned string is NOT guaranteed to be valid UTF-8
    % or UTF-16.
    %
:- pred read_line_as_string(io.result(string)::out, io::di, io::uo) is det.
:- pred read_line_as_string(io.text_input_stream::in, io.result(string)::out,
    io::di, io::uo) is det.

    % Discards all the whitespace characters satisfying char.is_whitespace
    % from the current stream or from the specified stream.
    %
:- pred ignore_whitespace(io.result::out, io::di, io::uo) is det.
:- pred ignore_whitespace(io.text_input_stream::in, io.result::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Bitmap input and output predicates.
%

    % Fill a bitmap from the current binary input stream
    % or from the specified binary input stream.
    % Return the number of bytes read. On end-of-file, the number of
    % bytes read will be less than the size of the bitmap, and
    % the result will be `ok'.
    % Throws an exception if the bitmap has a partial final byte.
    %
:- pred read_bitmap(bitmap::bitmap_di, bitmap::bitmap_uo,
    int::out, io.res::out, io::di, io::uo) is det.
:- pred read_bitmap(io.binary_input_stream::in,
    bitmap::bitmap_di, bitmap::bitmap_uo,
    int::out, io.res::out, io::di, io::uo) is det.
:- pragma obsolete(pred(read_bitmap/6), [bitmap.read_bitmap/6]).
:- pragma obsolete(pred(read_bitmap/7), [bitmap.read_bitmap/7]).

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
:- pragma obsolete(pred(read_bitmap/8), [bitmap.read_bitmap_range/8]).
:- pragma obsolete(pred(read_bitmap/9), [bitmap.read_bitmap_range/9]).

%---------------------%

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
:- pragma obsolete(pred(write_bitmap/3), [bitmap.write_bitmap/3]).
:- pragma obsolete(pred(write_bitmap/4), [bitmap.write_bitmap/4]).

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
:- pragma obsolete(pred(write_bitmap/5), [bitmap.write_bitmap_range/5]).
:- pragma obsolete(pred(write_bitmap/6), [bitmap.write_bitmap_range/6]).

%---------------------------------------------------------------------------%
%
% Reading values of arbitrary types.
%

    % Read a ground term of any type, written using standard Mercury syntax,
    % from the current stream or from the specified input stream.
    % The type of the term read is determined by the context from which
    % io.read is called.
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
    % See char.is_whitespace for the definition of whitespace characters
    % used by this predicate.
    %
:- pred read(io.read_result(T)::out, io::di, io::uo) is det.
:- pred read(io.text_input_stream::in, io.read_result(T)::out,
    io::di, io::uo) is det.

    % The type `posn' represents a position within a string.
    %
:- type posn
    --->    posn(
                % The first two fields are used only for computing
                % term contexts, for use e.g. in error messages.
                %
                % Line numbers start at 1; offsets start at zero.
                % So the usual posn at the start of a file is posn(1, 0, 0).
                % You can write it yourself, or get it by calling init_posn.
                posn_current_line_number        :: int,
                posn_offset_of_start_of_line    :: int,
                posn_current_offset             :: int
            ).

:- func init_posn = posn.

    % read_from_string(FileName, String, MaxPos, Result, Posn0, Posn):
    %
    % Does the same job as read/4, but reads from a string, not from a stream.
    %
    % FileName is the name of the source (for use in error messages).
    % String is the string to be parsed.
    % Posn0 is the position to start parsing from.
    % Posn is the position one past where the term read in ends.
    % MaxPos is the offset in the string which should be considered the
    % end-of-stream -- this is the upper bound for Posn.
    % (In the usual case, MaxPos is just the length of the String.)
    % WARNING: if MaxPos > length of String, then the behaviour is UNDEFINED.
    %
:- pred read_from_string(string::in, string::in, int::in, read_result(T)::out,
    posn::in, posn::out) is det.

    % Reads a binary representation of a term of type T from the current
    % binary input stream or from the specified binary input stream.
    %
    % Note: if you attempt to read a binary representation written by
    % a different program, or a different version of the same program,
    % then the results are not guaranteed to be meaningful. Another caveat
    % is that higher-order types cannot be read. (If you try, you will get
    % a runtime error.)
    %
    % XXX Note also that in the current implementation, read_binary
    % will not work on the Java back-end.
    %
:- pred read_binary(io.result(T)::out, io::di, io::uo) is det.
:- pred read_binary(io.binary_input_stream::in, io.result(T)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Writing values of arbitrary types.
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
    % escapes if necessary and backslash or octal escapes for all characters
    % for which char.is_control/1 is true. For higher-order types, or for types
    % defined using the foreign language interface (pragma foreign_type), the
    % text output will only describe the type that is being printed, not the
    % value, and the result may not be parsable by `read'. For the types
    % containing existential quantifiers, the type `type_desc' and closure
    % types, the result may not be parsable by `read', either. But in all other
    % cases the format used is standard Mercury syntax, and if you append a
    % period and newline (".\n"), then the results can be read in again using
    % `read'.
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

:- pred write_cc(T::in, io::di, io::uo) is cc_multi.
:- pred write_cc(io.text_output_stream::in, T::in, io::di, io::uo) is cc_multi.

    % write_line calls write and then writes a newline character.
    %
:- pred write_line(T::in, io::di, io::uo) is det.
:- pred write_line(io.text_output_stream::in, T::in, io::di, io::uo) is det.

:- pred write_line(io.text_output_stream, deconstruct.noncanon_handling, T,
    io, io).
:- mode write_line(in, in(do_not_allow), in, di, uo) is det.
:- mode write_line(in, in(canonicalize), in, di, uo) is det.
:- mode write_line(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write_line(in, in, in, di, uo) is cc_multi.

:- pred write_line_cc(T::in, io::di, io::uo) is cc_multi.
:- pred write_line_cc(io.text_output_stream::in, T::in, io::di, io::uo)
    is cc_multi.

    % Writes a binary representation of a term to the current binary output
    % stream or to the specified stream, in a format suitable for reading in
    % again with read_binary.
    %
:- pred write_binary(T::in, io::di, io::uo) is det.
:- pred write_binary(io.binary_output_stream::in, T::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Formatted output.
%

    % Formats the specified arguments according to the format string,
    % using string.format, and then writes the result to the current
    % output stream or to the specified output stream.
    % (See the documentation of string.format for details.)
    %
:- pred format(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pred format(io.text_output_stream::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Writing out several values.
%

    % Writes a list of strings to the current output stream
    % or to the specified output stream.
    %
:- pred write_strings(list(string)::in, io::di, io::uo) is det.
:- pred write_strings(io.text_output_stream::in, list(string)::in,
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

%---------------------------------------------------------------------------%
%
% Flushing output to the operating system.
%

    % Flush the output buffer of the current output stream
    % or to the specified output stream.
    %
:- pred flush_output(io::di, io::uo) is det.
:- pred flush_output(io.text_output_stream::in, io::di, io::uo) is det.

    % Flush the output buffer of the current binary output stream.
    % or of the specified binary output stream.
    %
:- pred flush_binary_output(io::di, io::uo) is det.
:- pred flush_binary_output(io.binary_output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Whole file input predicates.
%

    % Open and read the named file, and if successful, return its contents
    % as a string. If either the opening or the reading fails, return
    % an error message describing the failure.
    %
    % WARNING: the returned string is NOT guaranteed to be valid UTF-8
    % or UTF-16.
    %
:- pred read_named_file_as_string(string::in, io.res(string)::out,
    io::di, io::uo) is det.

    % Open and read the named file, and if successful, return its contents
    % as a list of lines. If either the opening or the reading fails, return
    % an error message describing the failure.
    %
    % This predicate views files as consisting of a sequence of lines,
    % with each line consisting of a possibly empty sequence of non-newline
    % characters, followed either by a newline character, or by the
    % end of the file. The string returned for each line will not contain
    % the newline character.
    %
    % WARNING: the returned string is NOT guaranteed to be valid UTF-8
    % or UTF-16.
    %
:- pred read_named_file_as_lines(string::in, io.res(list(string))::out,
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
    % WARNING: the returned string is NOT guaranteed to be valid UTF-8
    % or UTF-16.
    %
:- pred read_file_as_string(io.maybe_partial_res(string)::out,
    io::di, io::uo) is det.
:- pred read_file_as_string(io.text_input_stream::in,
    io.maybe_partial_res(string)::out, io::di, io::uo) is det.

    % The same as read_file_as_string, but returns not only a string,
    % but also the number of code units in that string.
    %
    % WARNING: the returned string is NOT guaranteed to be valid UTF-8
    % or UTF-16.
    %
:- pred read_file_as_string_and_num_code_units(
    io.maybe_partial_res_2(string, int)::out, io::di, io::uo) is det.
:- pred read_file_as_string_and_num_code_units(io.text_input_stream::in,
    io.maybe_partial_res_2(string, int)::out, io::di, io::uo) is det.

    % Reads all the bytes until eof or error from the current binary input
    % stream or from the specified binary input stream.
    %
:- pred read_binary_file(
    io.result(list(int))::out, io::di, io::uo) is det.
:- pred read_binary_file(io.binary_input_stream::in,
    io.result(list(int))::out, io::di, io::uo) is det.

    % Reads all the bytes until eof or error from the current binary input
    % stream or from the specified binary input stream into a bitmap.
    %
:- pred read_binary_file_as_bitmap(
    io.res(bitmap)::out, io::di, io::uo) is det.
:- pred read_binary_file_as_bitmap(io.binary_input_stream::in,
    io.res(bitmap)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Processing the contents of a whole file.
%

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred input_stream_foldl(pred(char, T, T), T, io.maybe_partial_res(T),
    io, io).
:- mode input_stream_foldl((pred(in, in, out) is det), in, out,
    di, uo) is det.
:- mode input_stream_foldl((pred(in, in, out) is cc_multi), in, out,
    di, uo) is cc_multi.
:- pragma obsolete(pred(input_stream_foldl/5), [stream.input_stream_fold/6]).

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error.
    %
:- pred input_stream_foldl(io.text_input_stream, pred(char, T, T),
    T, io.maybe_partial_res(T), io, io).
:- mode input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.
:- pragma obsolete(pred(input_stream_foldl/6), [stream.input_stream_fold/6]).

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred input_stream_foldl_io(pred(char, io, io), io.res, io, io).
:- mode input_stream_foldl_io((pred(in, di, uo) is det), out, di, uo)
    is det.
:- mode input_stream_foldl_io((pred(in, di, uo) is cc_multi), out, di, uo)
    is cc_multi.
:- pragma obsolete(pred(input_stream_foldl_io/4),
    [stream.input_stream_fold_state/5]).

    % Applies the given closure to each character (code point) read from the
    % input stream in turn, until eof or error.
    %
:- pred input_stream_foldl_io(io.text_input_stream, pred(char, io, io),
    io.res, io, io).
:- mode input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.
:- pragma obsolete(pred(input_stream_foldl_io/5),
    [stream.input_stream_fold_state/5]).

    % Applies the given closure to each character (code point) read from
    % the input stream in turn, until eof or error.
    %
:- pred input_stream_foldl2_io(pred(char, T, T, io, io),
    T, io.maybe_partial_res(T), io, io).
:- mode input_stream_foldl2_io((pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode input_stream_foldl2_io((pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.
:- pragma obsolete(pred(input_stream_foldl2_io/5),
    [stream.input_stream_fold_state/6]).

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
:- pragma obsolete(pred(input_stream_foldl2_io/6),
    [stream.input_stream_fold_state/6]).

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
:- pragma obsolete(pred(input_stream_foldl2_io_maybe_stop/5),
    [stream.input_stream_fold2_state_maybe_stop/6]).

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
:- pragma obsolete(pred(input_stream_foldl2_io_maybe_stop/5),
    [stream.input_stream_fold2_state_maybe_stop/6]).

%---------------------%

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl(pred(int, T, T),
    T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl((pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl((pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.
:- pragma obsolete(pred(binary_input_stream_foldl/5),
    [stream.input_stream_fold/6]).

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl(io.binary_input_stream,
    pred(int, T, T), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode binary_input_stream_foldl(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.
:- pragma obsolete(pred(binary_input_stream_foldl/6),
    [stream.input_stream_fold/6]).

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl_io(pred(int, io, io),
    io.res, io, io).
:- mode binary_input_stream_foldl_io((pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode binary_input_stream_foldl_io((pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.
:- pragma obsolete(pred(binary_input_stream_foldl_io/4),
    [stream.input_stream_fold_state/6]).

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl_io(io.binary_input_stream,
    pred(int, io, io), io.res, io, io).
:- mode binary_input_stream_foldl_io(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode binary_input_stream_foldl_io(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.
:- pragma obsolete(pred(binary_input_stream_foldl_io/5),
    [stream.input_stream_fold_state/6]).

    % Applies the given closure to each byte read from the current binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl2_io(
    pred(int, T, T, io, io), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io(
    in(pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io(
    in(pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.
:- pragma obsolete(pred(binary_input_stream_foldl2_io/5),
    [stream.input_stream_fold2_state/6]).

    % Applies the given closure to each byte read from the given binary
    % input stream in turn, until eof or error.
    %
:- pred binary_input_stream_foldl2_io(io.binary_input_stream,
    pred(int, T, T, io, io), T, maybe_partial_res(T), io, io).
:- mode binary_input_stream_foldl2_io(in,
    (pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode binary_input_stream_foldl2_io(in,
    (pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.
:- pragma obsolete(pred(binary_input_stream_foldl2_io/6),
    [stream.input_stream_fold2_state/6]).

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
:- pragma obsolete(pred(binary_input_stream_foldl2_io_maybe_stop/5),
    [stream.input_stream_fold2_state_maybe_stop/6]).

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
:- pragma obsolete(pred(binary_input_stream_foldl2_io_maybe_stop/6),
    [stream.input_stream_fold2_state_maybe_stop/6]).

%---------------------------------------------------------------------------%
%
% File handling predicates.
%

% NOTE_TO_IMPLEMENTORS The definitions of access_type and file_type logically
% NOTE_TO_IMPLEMENTORS belong in io.file.m, and they should be moved there
% NOTE_TO_IMPLEMENTORS when the forwarding predicates below to io.file.m
% NOTE_TO_IMPLEMENTORS are removed. However, while the forwarding predicates
% NOTE_TO_IMPLEMENTORS are here, the types that some of them depend on
% NOTE_TO_IMPLEMENTORS should be here as well.
% NOTE_TO_IMPLEMENTORS
% NOTE_TO_IMPLEMENTORS Keeping the definitions of these types here suits both
% NOTE_TO_IMPLEMENTORS old code and new code.
% NOTE_TO_IMPLEMENTORS
% NOTE_TO_IMPLEMENTORS - Old code can call all the forwarding predicates,
% NOTE_TO_IMPLEMENTORS   and pass them values of these types, while importing
% NOTE_TO_IMPLEMENTORS   only io.m.
% NOTE_TO_IMPLEMENTORS
% NOTE_TO_IMPLEMENTORS - New code using io.file.m must also import io.m anyway,
% NOTE_TO_IMPLEMENTORS   not just because io.m is io.file.m's parent
% NOTE_TO_IMPLEMENTORS   (a requirement that we may eliminate in the future),
% NOTE_TO_IMPLEMENTORS   but because all the operations in io.file.m take
% NOTE_TO_IMPLEMENTORS   a pair of I/O state arguments, and the I/O state type
% NOTE_TO_IMPLEMENTORS   is defined here.
% NOTE_TO_IMPLEMENTORS
% NOTE_TO_IMPLEMENTORS On the other hand, if we moved the definitions of
% NOTE_TO_IMPLEMENTORS these types to io.file.m, new code would still work,
% NOTE_TO_IMPLEMENTORS but old code would break immediately, in a way that
% NOTE_TO_IMPLEMENTORS shutting up warnings about obsolete predicates
% NOTE_TO_IMPLEMENTORS would not overcode.
:- type access_type
    --->    read
    ;       write
    ;       execute.

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

    % remove_file(FileName, Result, !IO) attempts to remove the file
    % FileName, binding Result to ok/0 if it succeeds, or error/1 if it
    % fails. If FileName names a file that is currently open, the behaviour
    % is implementation-dependent.
    %
    % If FileName names a directory, the behavior is currently
    % implementation-dependent. On most platforms, an empty directory will be
    % deleted.
    %
:- pred remove_file(string::in, io.res::out, io::di, io::uo) is det.
:- pragma obsolete(pred(remove_file/4), [io.file.remove_file/4]).

    % remove_file_recursively(FileName, Result, !IO) attempts to remove
    % the file FileName, binding Result to ok/0 if it succeeds, or error/1
    % if it fails. If FileName names a file that is currently open, the
    % behaviour is implementation-dependent.
    %
    % Unlike remove_file, this predicate will attempt to remove non-empty
    % directories (recursively). If it fails, some of the directory elements
    % may already have been removed.
    %
:- pred remove_file_recursively(string::in, io.res::out, io::di, io::uo)
    is det.
:- pragma obsolete(pred(remove_file_recursively/4),
    [io.file.remove_file_recursively/4]).

    % rename_file(OldFileName, NewFileName, Result, !IO).
    %
    % Attempts to rename the file or directory OldFileName as NewFileName,
    % binding Result to ok/0 if it succeeds, or error/1 if it fails.
    % If OldFileName names a file that is currently open, the behaviour is
    % implementation-dependent. If NewFileName names a file that already
    % exists the behaviour is also implementation-dependent; on some systems,
    % the file previously named NewFileName will be deleted and replaced
    % with the file previously named OldFileName.
    %
:- pred rename_file(string::in, string::in, io.res::out, io::di, io::uo)
    is det.
:- pragma obsolete(pred(rename_file/5), [io.file.rename_file/5]).

%---------------------%

    % Succeeds if this platform can read and create symbolic links.
    %
:- pred have_symlinks is semidet.
:- pragma obsolete(pred(have_symlinks/0), [io.file.have_symlinks/0]).

    % make_symlink(FileName, LinkFileName, Result, !IO).
    %
    % Attempts to make LinkFileName be a symbolic link to FileName.
    % If FileName is a relative path, it is interpreted relative
    % to the directory containing LinkFileName.
    %
:- pred make_symlink(string::in, string::in, io.res::out, io::di, io::uo)
    is det.
:- pragma obsolete(pred(make_symlink/5), [io.file.make_symlink/5]).

    % read_symlink(FileName, Result, !IO) returns `ok(LinkTarget)'
    % if FileName is a symbolic link pointing to LinkTarget, and
    % `error(Error)' otherwise. If LinkTarget is a relative path,
    % it should be interpreted relative the directory containing FileName,
    % not the current directory.
    %
:- pred read_symlink(string::in, io.res(string)::out, io::di, io::uo) is det.
:- pragma obsolete(pred(read_symlink/4), [io.file.read_symlink/4]).

%---------------------%

    % check_file_accessibility(FileName, AccessTypes, Result):
    %
    % Check whether the current process can perform the operations given
    % in AccessTypes on FileName.
    % XXX When using the .NET CLI, this predicate will sometimes report
    % that a directory is writable when in fact it is not.
    %
:- pred check_file_accessibility(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.
:- pragma obsolete(pred(check_file_accessibility/5),
    [io.file.check_file_accessibility/5]).

    % file_type(FollowSymLinks, FileName, TypeResult)
    % finds the type of the given file.
    %
:- pred file_type(bool::in, string::in, io.res(file_type)::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(file_type/5), [io.file.file_type/5]).

    % file_modification_time(FileName, TimeResult)
    % finds the last modification time of the given file.
    %
:- pred file_modification_time(string::in, io.res(time_t)::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(file_modification_time/4),
    [io.file.file_modification_time/4]).

%---------------------------------------------------------------------------%
%
% Predicates for handling temporary files.
%

    % make_temp_file(Result, !IO) creates an empty file whose name is different
    % to the name of any existing file. If successful Result returns the name
    % of the file. It is the responsibility of the caller to delete the file
    % when it is no longer required.
    %
    % The file is placed in the directory returned by get_temp_directory/3.
    %
    % On the Java backend, this does not attempt to create the file
    % with restrictive permissions (600 on Unix-like systems) and therefore
    % should not be used when security is required.
    %
:- pred make_temp_file(io.res(string)::out, io::di, io::uo) is det.
:- pragma obsolete(pred(make_temp_file/3), [io.file.make_temp_file/3]).

    % make_temp_file(Dir, Prefix, Suffix, Result, !IO) creates an empty file
    % whose name is different to the name of any existing file. The file will
    % reside in the directory specified by Dir and will have a prefix using up
    % to the first 5 code units of Prefix. If successful, Result returns the
    % name of the file. It is the responsibility of the caller to delete the
    % file when it is no longer required.
    %
    % The reason for truncating Prefix is historical; in future the behaviour
    % may be changed. Note that the truncation is performed without regard for
    % code point boundaries. It is recommended to use only (printable) ASCII
    % characters in the prefix string.
    %
    % The C backend has the following limitations:
    %   - Suffix may be ignored.
    %
    % The C# backend has the following limitations:
    %   - Dir is ignored.
    %   - Prefix is ignored.
    %   - Suffix is ignored.
    %
    % On the Java backend, this does not attempt to create the file
    % with restrictive permissions (600 on Unix-like systems) and therefore
    % should not be used when security is required.
    %
:- pred make_temp_file(string::in, string::in, string::in, io.res(string)::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(make_temp_file/6), [io.file.make_temp_file/6]).

    % make_temp_directory(Result, !IO) creates an empty directory whose name
    % is different from the name of any existing directory.
    %
    % On the Java backend this is insecure as the file permissions are not set.
    %
:- pred make_temp_directory(io.res(string)::out, io::di, io::uo) is det.
:- pragma obsolete(pred(make_temp_directory/3),
    [io.file.make_temp_directory/3]).

    % make_temp_directory(Dir, Prefix, Suffix, Result, !IO) creates an empty
    % directory whose name is different from the name of any existing
    % directory. The new directory will reside in the existing directory
    % specified by Dir and will have a prefix using up to the first 5
    % characters of Prefix and a Suffix. Result returns the name of the
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
:- pred make_temp_directory(string::in, string::in, string::in,
    io.res(string)::out, io::di, io::uo) is det.
:- pragma obsolete(pred(make_temp_directory/6),
    [io.file.make_temp_directory/6]).

    % Test if the make_temp_directory predicates are available.
    % This is false for C backends without support for mkdtemp(3).
    %
:- pred have_make_temp_directory is semidet.
:- pragma obsolete(pred(have_make_temp_directory/0),
    [io.file.have_make_temp_directory/0]).

    % get_temp_directory(DirName, !IO)
    %
    % DirName is the name of a directory where applications should put
    % temporary files.
    %
    % This is implementation-dependent. For current Mercury implementations,
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
:- pragma obsolete(pred(get_temp_directory/3), [io.file.get_temp_directory/3]).

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

%---------------------%

    % The I/O state contains an integer used to record the program's exit
    % status. When the program finishes, it will return this exit status
    % to the operating system. The following predicates can be used to get
    % and set the exit status.
    %
:- pred get_exit_status(int::out, io::di, io::uo) is det.
:- pred set_exit_status(int::in, io::di, io::uo) is det.

%---------------------%

% NOTE_TO_IMPLEMENTORS This type is here for the same reason that
% NOTE_TO_IMPLEMENTORS the access_type type is here. See the comment there.
    % Values of this type map the names of environment variables
    % to their values.
    %
:- type environment_var_map == map(string, string).

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
:- pragma obsolete(pred(get_environment_var/4),
    [io.environment.get_environment_var/4]).

    % First argument is the name of the environment variable, second argument
    % is the value to be assigned to that variable. Res is 'ok' on success or
    % 'error(ErrorCode)' if the system runs out of environment space or if
    % the environment cannot be modified.
    %
    % Note that the environment cannot be modified on Java.
    %
:- pred set_environment_var(string::in, string::in, io.res::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(set_environment_var/5),
    [io.environment.set_environment_var/5]).

    % Same as set_environment_var/5, but throws an exception if an error
    % occurs.
    %
:- pred set_environment_var(string::in, string::in, io::di, io::uo) is det.
:- pragma obsolete(pred(set_environment_var/4),
    [io.environment.set_environment_var/4]).

    % Test if the set_environment_var/{4,5} predicates are available.
    % This is false for Java backends.
    %
:- pred have_set_environment_var is semidet.
:- pragma obsolete(pred(have_set_environment_var/0),
    [io.environment.have_set_environment_var/0]).

    % Return a map containing all the environment variables in the current
    % environment, together with their values.
    %
:- pred get_environment_var_map(environment_var_map::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(get_environment_var_map/3),
    [io.environment.get_environment_var_map/3]).

%---------------------------------------------------------------------------%
%
% System access predicates.
%

% NOTE_TO_IMPLEMENTORS This type is here for the same reason that
% NOTE_TO_IMPLEMENTORS the access_type type is here. See the comment there.
:- type system_result
    --->    exited(int)
    ;       signalled(int).

    % Invokes the operating system shell with the specified Command.
    % Result is either `ok(ExitStatus)', if it was possible to invoke
    % the command, or `error(ErrorCode)' if not. The ExitStatus will be 0
    % if the command completed successfully or the return value of the system
    % call. If a signal kills the system call, then Result will be an error
    % indicating which signal occurred.
    %
:- pred call_system(string::in, io.res(int)::out, io::di, io::uo) is det.
:- pragma obsolete(pred(call_system_return_signal/4),
    [io.call_system.call_system/4]).

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
:- pred call_system_return_signal(string::in, io.res(system_result)::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(call_system_return_signal/4),
    [io.call_system.call_system_return_signal/4]).

%---------------------------------------------------------------------------%
%
% Managing the globals structure that Mercury attaches to the I/O state.
%

    % The I/O state includes a `globals' field which is not used by the
    % standard library, but can be used by the application. The globals field
    % is of type univ so that the application can store any data it wants
    % there. The following predicates can be used to access this global state.
    %
    % Does not modify the I/O state.
    %
    % The globals field is obsolete. A mutable declaration will provide
    % the same functionality with better type safety.
    %
:- pred get_globals(univ::out, io::di, io::uo) is det.
:- pred set_globals(univ::in, io::di, io::uo) is det.
:- pragma obsolete(pred(get_globals/3)).
:- pragma obsolete(pred(set_globals/3)).

    % update_globals(UpdatePred, !IO).
    % Update the globals field in the I/O state based upon its current value.
    % This is equivalent to the following:
    %
    %   get_globals(Globals0, !IO),
    %   UpdatePred(Globals0, Globals),
    %   set_globals(Globals, !IO)
    %
    % In parallel grades calls to update_globals/3 are atomic.
    % If UpdatePred throws an exception then the globals field is
    % left unchanged.
    %
    % The globals field is obsolete. A mutable declaration will provide
    % the same functionality with better type safety.
    %
:- pred update_globals(pred(univ, univ)::in(pred(in, out) is det),
    io::di, io::uo) is det.
:- pragma obsolete(pred(update_globals/3)).

%---------------------------------------------------------------------------%
%
% Predicates that report statistics about the execution of the current process
% so far.
%

    % report_stats(Stream, Selector, !IO):
    % report_stats(Selector, !IO):
    %
    % Write selected statistics to the specified stream, or (if none)
    % to stderr. What statistics will be written is controlled by the
    % Selector argument. What selector values cause what statistics
    % to be printed is implementation defined.
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
:- pred report_stats(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.
:- pred report_stats(string::in, io::di, io::uo) is det.
:- pragma obsolete(pred(report_stats/4), [benchmarking.report_stats/4]).
:- pragma obsolete(pred(report_stats/3), [benchmarking.report_stats/3]).

    % Write standard memory/time usage statistics to the specified stream,
    % or (if none) to stderr.
    %
:- pred report_standard_stats(io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred report_standard_stats(io::di, io::uo) is det.
:- pragma obsolete(pred(report_standard_stats/3),
    [benchmarking.report_standard_stats/3]).
:- pragma obsolete(pred(report_standard_stats/2),
    [benchmarking.report_standard_stats/2]).

    % report_full_memory_stats/3 reports a full memory profile
    % to the specified output stream, or (if none) to stderr.
    %
:- pred report_full_memory_stats(io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred report_full_memory_stats(io::di, io::uo) is det.
:- pragma obsolete(pred(report_full_memory_stats/3),
    [benchmarking.report_full_memory_stats/3]).
:- pragma obsolete(pred(report_full_memory_stats/2),
    [benchmarking.report_full_memory_stats/2]).

    % report_tabling_statistics/3, as its name says, reports statistics
    % about tabling to the specified output stream, or (if none) to stderr.
    %
    % XXX For now, these predicates work only with the C backend.
    %
:- pred report_tabling_statistics(io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred report_tabling_statistics(io::di, io::uo) is det.
:- pragma obsolete(pred(report_tabling_statistics/3),
    [benchmarking.report_tabling_statistics/3]).
:- pragma obsolete(pred(report_tabling_statistics/2),
    [benchmarking.report_tabling_statistics/2]).

%---------------------------------------------------------------------------%
%
% Interpreting I/O error messages.
%

    % Construct an error value with the specified error message.
    % The error value will not have an associated system error.
    %
:- func make_io_error(string) = io.error.

    % make_io_error_from_system_error(SystemError, Prefix, Error, !IO):
    %
    % Construct an io.error value given a system error and an error message
    % prefix, which may be the empty string. The error message will be
    % constructed by appending Prefix and the error message retrieved from the
    % system for SystemError.
    %
    % On C backends, the io.system_error must be an errno value,
    % not a Windows error code.
    %
:- pred make_io_error_from_system_error(io.system_error::in, string::in,
    io.error::out, io::di, io::uo) is det.

    % make_io_error_from_windows_error(SystemError, Prefix, Error, !IO):
    %
    % Construct an io.error value from the given Windows system error and error
    % message prefix. This predicate may only be called when using a C backend
    % running on Windows. On other platforms, it throws an exception.
    %
:- pred make_io_error_from_windows_error(io.system_error::in, string::in,
    io.error::out, io::di, io::uo) is det.

    % Return an error message for the error value.
    %
:- func error_message(io.error) = string.
:- pred error_message(io.error::in, string::out) is det.

    % get_system_error(Error, SystemError):
    %
    % Succeeds iff SystemError is a system error associated with Error.
    %
:- pred get_system_error(io.error::in, io.system_error::out) is semidet.

    % As above, but only succeeds if the system error is an errno value.
    %
:- pred get_errno_error(io.error::in, io.system_error::out) is semidet.

    % As above, but only succeeds if the system error is a Windows error code.
    %
:- pred get_windows_error(io.error::in, io.system_error::out) is semidet.

    % As above, but only if the system error is a C# or Java exception object,
    % or null.
    %
:- pred get_exception_object_error(io.error::in, io.system_error::out)
    is semidet.

    % get_system_error_name(Error, ErrorName):
    %
    % Succeeds if Error has an associated system error, otherwise fails.
    % On success, ErrorName is a name for that system error as follows.
    %
    % For C backends, a system error is usually an errno value. If the errno
    % value is recognised by the Mercury system, then ErrorName will be the
    % name for that errno value as defined in <errno.h>, e.g. "ENOENT".
    % Otherwise, ErrorName will be "errno N" where N is a decimal number.
    %
    % For C backends on Windows, a system error may instead be a Windows system
    % error code. If the error code is recognised by the Mercury system, then
    % ErrorName will be the name for that error code in the Windows API,
    % e.g. "ERROR_FILE_NOT_FOUND". Otherwise, ErrorName will be
    % "System error 0xN" where 0xN is a hexadecimal number.
    %
    % For the C# backend, ErrorName will be the fully qualified class name
    % of an exception object, e.g. "System.IO.FileNotFoundException",
    % or "null".
    %
    % For the Java backend, ErrorName will be the fully qualified class name
    % of an exception object, e.g. "java.io.FileNotFoundException",
    % or "null".
    %
:- pred get_system_error_name(io.error::in, string::out) is semidet.

%---------------------------------------------------------------------------%
%
% Instances of the stream typeclasses.
%

:- instance stream.error(io.error).

% Text input stream instances.
:- instance stream.stream(text_input_stream,        io).
:- instance stream.input(text_input_stream,         io).
:- instance stream.line_oriented(text_input_stream, io).

:- instance stream.reader(text_input_stream,         char,      io, io.error).
:- instance stream.reader(text_input_stream,         line,      io, io.error).
:- instance stream.reader(text_input_stream,         text_file, io, io.error).

:- instance stream.unboxed_reader(text_input_stream, char,      io, io.error).

:- instance stream.putback(text_input_stream,        char,      io, io.error).

% Binary input stream instances.
:- instance stream.stream(binary_input_stream,      io).
:- instance stream.input(binary_input_stream,       io).
:- instance stream.seekable(binary_input_stream,    io).

:- instance stream.reader(binary_input_stream,         int,   io, io.error).
:- instance stream.reader(binary_input_stream,         int8,  io, io.error).
:- instance stream.reader(binary_input_stream,         uint8, io, io.error).
% The instance for int, which predates the addition of sized integers
% to Mercury, reads a byte, which it stores in the least significant
% eight bits. The plan is to eventually delete the instance for int.

:- instance stream.unboxed_reader(binary_input_stream, int8,  io, io.error).
:- instance stream.unboxed_reader(binary_input_stream, uint8, io, io.error).
% This typeclass was defined after the addition of sized integers
% to Mercury, which is why there is no instance for int.

:- instance stream.putback(binary_input_stream,        int,   io, io.error).
:- instance stream.putback(binary_input_stream,        int8,  io, io.error).
:- instance stream.putback(binary_input_stream,        uint8, io, io.error).
% The instance for int, which predates the addition of sized integers
% to Mercury, puts back a byte, which it stores in the least significant
% eight bits. The plan is to eventually delete the instance for int.

% Text output stream instances.
:- instance stream.stream(text_output_stream,        io).
:- instance stream.output(text_output_stream,        io).
:- instance stream.line_oriented(text_output_stream, io).

:- instance stream.writer(text_output_stream, char,   io).
:- instance stream.writer(text_output_stream, float,  io).
:- instance stream.writer(text_output_stream, int,    io).
:- instance stream.writer(text_output_stream, int8,   io).
:- instance stream.writer(text_output_stream, int16,  io).
:- instance stream.writer(text_output_stream, int32,  io).
:- instance stream.writer(text_output_stream, int64,  io).
:- instance stream.writer(text_output_stream, uint,   io).
:- instance stream.writer(text_output_stream, uint8,  io).
:- instance stream.writer(text_output_stream, uint16, io).
:- instance stream.writer(text_output_stream, uint32, io).
:- instance stream.writer(text_output_stream, uint64, io).
:- instance stream.writer(text_output_stream, string, io).
:- instance stream.writer(text_output_stream, univ,   io).

% Binary output stream instances.
:- instance stream.stream(binary_output_stream, io).
:- instance stream.output(binary_output_stream, io).
:- instance stream.seekable(binary_output_stream, io).

:- instance stream.writer(binary_output_stream, byte,  io).
:- instance stream.writer(binary_output_stream, int8,  io).
:- instance stream.writer(binary_output_stream, uint8, io).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%---------------------------------------------------------------------------%

:- interface.

:- include_module error_util.
:- include_module primitives_read.  % Include exported for symmetry.
:- include_module primitives_write. % Include exported for benchmarking.m.
:- include_module stream_db.        % Include exported for browser/browse.m.
:- include_module stream_ops.       % Include exported for benchmarking.m.
:- include_module text_read.        % Include exported for symmetry.

%---------------------%
%
% For use by library.m.
%

:- pred init_state(io::di, io::uo) is det.
:- pred finalize_state(io::di, io::uo) is det.

%---------------------%
%
% For use by dir.m.
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

    % Return a unique identifier for the given file (after following
    % symlinks in FileName).
    % XXX On Cygwin sometimes two files will have the same file_id.
    % This is because MS-Windows does not use inodes, so Cygwin hashes
    % the absolute file name. On Windows without Cygwin this will always
    % return error(_). That does not matter, because this function is
    % only used for checking for symlink loops in dir.foldl2, but
    % plain Windows doesn't support symlinks.
    %
:- type file_id.
:- pred file_id(string::in, io.res(file_id)::out, io::di, io::uo) is det.

%---------------------%
%
% For use by bitmap.m.
%

:- type stream.
:- func input_stream_get_stream(input_stream) = stream.
:- func output_stream_get_stream(output_stream) = stream.
:- func binary_input_stream_get_stream(binary_input_stream) = stream.
:- func binary_output_stream_get_stream(binary_output_stream) = stream.

%---------------------%
%
% For use by the compiler transformation that implements trace [io(!IO)].
% Both of these are builtins.
%

:- semipure pred unsafe_get_io_state(io::uo) is det.
:- impure pred unsafe_set_io_state(io::di) is det.

%---------------------%
%
% Workaround for problem in commit 318e708.
% XXX Not exporting a declaration for this type causes a compiler abort
% when installing the java grade.

:- type stream_id.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module dir.
:- import_module exception.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module io.call_system.
:- import_module io.environment.
:- import_module io.error_util.
:- import_module io.file.
:- import_module io.primitives_read.
:- import_module io.primitives_write.
:- import_module io.stream_db.
:- import_module io.stream_ops.
:- import_module io.text_read.
:- import_module mercury_term_parser.
:- import_module require.
:- import_module stream.string_writer.
:- import_module term.
:- import_module term_conversion.
:- import_module term_subst.
:- import_module type_desc.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

:- use_module rtti_implementation.
:- use_module table_builtin.

:- pragma foreign_import_module("C", time).   % For ML_construct_time_t.
:- pragma foreign_import_module("C", string).
% mercury_std{in,out,err} are defined in io.stream_ops.
% This foreign_import_module is needed with LIBRARY_INTERMODULE = no.
:- pragma foreign_import_module("C", io.stream_ops).

%---------------------------------------------------------------------------%
%
% Private type definitions.
%

    % Values of type io.state are never really used:
    % instead we store data in global variables.
    % The reason this is defined as a foreign type is to prevent attempts
    % to deconstruct values of the type.
:- pragma foreign_type("C", io.state, "MR_Word",
    [can_pass_as_mercury_type])
    where equality is io_state_equal, comparison is io_state_compare.
:- pragma foreign_type("C#", io.state, "int",
    [can_pass_as_mercury_type])
    where equality is io_state_equal, comparison is io_state_compare.
:- pragma foreign_type("Java", io.state, "java.lang.Object",
    [can_pass_as_mercury_type])
    where equality is io_state_equal, comparison is io_state_compare.

:- pred io_state_equal(io.state::in, io.state::in) is semidet.

:- pragma no_determinism_warning(pred(io_state_equal/2)).
io_state_equal(_, _) :-
    error("attempt to unify two I/O states").

:- pred io_state_compare(comparison_result::uo, io.state::in, io.state::in)
    is det.

:- pragma no_determinism_warning(pred(io_state_compare/3)).
io_state_compare(_, _, _) :-
    error("attempt to compare two I/O states").

:- type input_stream
    --->    input_stream(stream).
:- type output_stream
    --->    output_stream(stream).
% While these definitions of the input_stream and output_stream types
% are visible only in io.m for Mercury code, you can access the underlying
% stdio streams using MR_file(*MR_unwrap_{input,output}_stream(Stream))
% in C code.
%
% XXX Document any equivalent mechanisms for Java and C#.

:- type binary_input_stream
    --->    binary_input_stream(stream).
:- type binary_output_stream
    --->    binary_output_stream(stream).

:- type stream
    --->    stream(c_pointer).
:- pragma foreign_type("C", stream,
    "MercuryFilePtr", [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", stream,
    "mercury.io__stream_ops.MR_MercuryFileStruct").
:- pragma foreign_type("Java", stream,
    "jmercury.io__stream_ops.MR_MercuryFileStruct").

    % A unique identifier for an I/O stream.
    %
:- type stream_id == int.

%---------------------------------------------------------------------------%
%
% Opening and closing streams, both text and binary.
%

open_input(FileName, Result, !IO) :-
    do_open_text(FileName, "r", OpenCount, NewStream, Error, !IO),
    is_error(Error, "can't open input file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(input_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, input, text, file(FileName)), !IO)
    ).

open_binary_input(FileName, Result, !IO) :-
    do_open_binary(FileName, "rb", OpenCount, NewStream, Error, !IO),
    is_error(Error, "can't open input file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(binary_input_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, input, binary, file(FileName)), !IO)
    ).

%---------------------%

open_output(FileName, Result, !IO) :-
    do_open_text(FileName, "w", OpenCount, NewStream, Error, !IO),
    is_error(Error, "can't open output file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, output, text, file(FileName)), !IO)
    ).

open_binary_output(FileName, Result, !IO) :-
    do_open_binary(FileName, "wb", OpenCount, NewStream, Error, !IO),
    is_error(Error, "can't open output file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(binary_output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, output, binary, file(FileName)), !IO)
    ).

%---------------------%

open_append(FileName, Result, !IO) :-
    do_open_text(FileName, "a", OpenCount, NewStream, Error, !IO),
    is_error(Error, "can't append to file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, append, text, file(FileName)), !IO)
    ).

open_binary_append(FileName, Result, !IO) :-
    do_open_binary(FileName, "ab", OpenCount, NewStream, Error, !IO),
    is_error(Error, "can't append to file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(binary_output_stream(NewStream)),
        insert_stream_info(NewStream,
            stream(OpenCount, append, binary, file(FileName)), !IO)
    ).

%---------------------%

close_input(input_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

close_binary_input(binary_input_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

%---------------------%

close_output(output_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

close_binary_output(binary_output_stream(Stream), !IO) :-
    maybe_delete_stream_info(Stream, !IO),
    close_stream(Stream, Error, !IO),
    throw_on_close_error(Error, !IO).

%---------------------------------------------------------------------------%
%
% Switching streams.
%

set_input_stream(input_stream(NewStream), input_stream(OutStream), !IO) :-
    set_input_stream_2(NewStream, OutStream, !IO).

set_binary_input_stream(binary_input_stream(NewStream),
        binary_input_stream(OutStream), !IO) :-
    set_binary_input_stream_2(NewStream, OutStream, !IO).

%---------------------%

set_output_stream(output_stream(NewStream), output_stream(OutStream), !IO) :-
    set_output_stream_2(NewStream, OutStream, !IO).

set_binary_output_stream(binary_output_stream(NewStream),
        binary_output_stream(OutStream), !IO) :-
    set_binary_output_stream_2(NewStream, OutStream, !IO).

%---------------------------------------------------------------------------%
%
% Seeking on binary streams.
%

seek_binary_input(binary_input_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    seek_binary_2(Stream, Flag, int64.from_int(Offset), Error, !IO),
    throw_on_error(Error, "error seeking in file: ", !IO).

seek_binary_input64(binary_input_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    seek_binary_2(Stream, Flag, Offset, Error, !IO),
    throw_on_error(Error, "error seeking in file: ", !IO).

%---------------------%

seek_binary_output(binary_output_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    seek_binary_2(Stream, Flag, int64.from_int(Offset), Error, !IO),
    throw_on_error(Error, "error seeking in file: ", !IO).

seek_binary_output64(binary_output_stream(Stream), Whence, Offset, !IO) :-
    whence_to_int(Whence, Flag),
    seek_binary_2(Stream, Flag, Offset, Error, !IO),
    throw_on_error(Error, "error seeking in file: ", !IO).

%---------------------%

binary_input_stream_offset(binary_input_stream(Stream), Offset, !IO) :-
    binary_stream_offset_2(Stream, Offset64, Error, !IO),
    throw_on_error(Error, "error getting file offset: ", !IO),
    ( if int64.to_int(Offset64, OffsetPrime) then
        Offset = OffsetPrime
    else
        error("io.binary_input_stream_offset: offset exceeds range of an int")
    ).

binary_input_stream_offset64(binary_input_stream(Stream), Offset, !IO) :-
    binary_stream_offset_2(Stream, Offset, Error, !IO),
    throw_on_error(Error, "error getting file offset: ", !IO).

%---------------------%

binary_output_stream_offset(binary_output_stream(Stream), Offset, !IO) :-
    binary_stream_offset_2(Stream, Offset64, Error, !IO),
    throw_on_error(Error, "error getting file offset: ", !IO),
    ( if int64.to_int(Offset64, OffsetPrime) then
        Offset = OffsetPrime
    else
        error("io.binary_output_stream_offset: offset exceeds range of an int")
    ).

binary_output_stream_offset64(binary_output_stream(Stream), Offset, !IO) :-
    binary_stream_offset_2(Stream, Offset, Error, !IO),
    throw_on_error(Error, "error getting file offset: ", !IO).

%---------------------------------------------------------------------------%
%
% Standard stream id predicates.
%

stdin_stream = input_stream(stdin_stream_2).

stdin_stream(input_stream(Stream), !IO) :-
    stdin_stream_2(Stream, !IO).

stdin_binary_stream(binary_input_stream(Stream), !IO) :-
    stdin_binary_stream_2(Stream, !IO).

%---------------------%

stdout_stream = output_stream(stdout_stream_2).

stdout_stream(output_stream(Stream), !IO) :-
    stdout_stream_2(Stream, !IO).

stdout_binary_stream(binary_output_stream(Stream), !IO) :-
    stdout_binary_stream_2(Stream, !IO).

%---------------------%

stderr_stream = output_stream(stderr_stream_2).

stderr_stream(output_stream(Stream), !IO) :-
    stderr_stream_2(Stream, !IO).

%---------------------------------------------------------------------------%
%
% Current stream id predicates.
%

input_stream(input_stream(Stream), !IO) :-
    input_stream_2(Stream, !IO).

binary_input_stream(binary_input_stream(Stream), !IO) :-
    binary_input_stream_2(Stream, !IO).

%---------------------%

output_stream(output_stream(Stream), !IO) :-
    output_stream_2(Stream, !IO).

binary_output_stream(binary_output_stream(Stream), !IO) :-
    binary_output_stream_2(Stream, !IO).

%---------------------------------------------------------------------------%
%
% Getting and setting stream properties.
%

input_stream_name(Name, !IO) :-
    input_stream(input_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

input_stream_name(input_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

binary_input_stream_name(Name, !IO) :-
    binary_input_stream(binary_input_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

binary_input_stream_name(binary_input_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

%---------------------%

output_stream_name(Name, !IO) :-
    output_stream(output_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

output_stream_name(output_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

binary_output_stream_name(Name, !IO) :-
    binary_output_stream(binary_output_stream(Stream), !IO),
    stream_name(Stream, Name, !IO).

binary_output_stream_name(binary_output_stream(Stream), Name, !IO) :-
    stream_name(Stream, Name, !IO).

%---------------------------------------------------------------------------%

get_line_number(LineNum, !IO) :-
    input_stream_2(Stream, !IO),
    get_input_line_number_2(Stream, LineNum, !IO).

get_line_number(input_stream(Stream), LineNum, !IO) :-
    get_input_line_number_2(Stream, LineNum, !IO).

%---------------------%

set_line_number(LineNum, !IO) :-
    output_stream_2(Stream, !IO),
    set_input_line_number_2(Stream, LineNum, !IO).

set_line_number(input_stream(Stream), LineNum, !IO) :-
    set_input_line_number_2(Stream, LineNum,!IO).

%---------------------%

get_output_line_number(LineNum, !IO) :-
    output_stream_2(Stream, !IO),
    get_output_line_number_2(Stream, LineNum, !IO).

get_output_line_number(output_stream(Stream), LineNum, !IO) :-
    get_output_line_number_2(Stream, LineNum, !IO).

%---------------------%

set_output_line_number(LineNum, !IO) :-
    output_stream_2(Stream, !IO),
    set_output_line_number_2(Stream, LineNum, !IO).

set_output_line_number(output_stream(Stream), LineNum, !IO) :-
    set_output_line_number_2(Stream, LineNum, !IO).

%---------------------------------------------------------------------------%
%
% Reading values of primitive types.
%

:- pragma inline(pred(read_char/3)).          % Inline to allow deforestation.
read_char(Result, !IO) :-
    input_stream(Stream, !IO),
    read_char(Stream, Result, !IO).

:- pragma inline(pred(read_char/4)).          % Inline to allow deforestation.
read_char(Stream, Result, !IO) :-
    read_char_code(Stream, ResultCode, Error, Char, !IO),
    interpret_result_code1(ResultCode, Error, Char, Result, !IO).

:- pragma inline(pred(read_char_unboxed/5)).  % Inline to allow deforestation.
read_char_unboxed(Stream, Result, Char, !IO) :-
    read_char_code(Stream, ResultCode, Error, Char, !IO),
    interpret_result_code0(ResultCode, Error, Result, !IO).

%---------------------%

putback_char(Char, !IO) :-
    input_stream(Stream, !IO),
    putback_char(Stream, Char, !IO).

putback_char(input_stream(Stream), Character, !IO) :-
    putback_char_2(Stream, Character, Ok, !IO),
    (
        Ok = yes
    ;
        Ok = no,
        throw(io_error_string("failed to put back character"))
    ).

%---------------------%

:- pragma inline(pred(read_byte/3)).          % Inline to allow deforestation.
read_byte(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_byte(Stream, Result, !IO).

:- pragma inline(pred(read_byte/4)).          % Inline to allow deforestation.
read_byte(binary_input_stream(Stream), Result, !IO) :-
    read_byte_val(input_stream(Stream), ResultCode, Error, Byte, !IO),
    interpret_result_code1(ResultCode, Error, Byte, Result, !IO).

read_binary_int8(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int8(Stream, Result, !IO).

read_binary_int8(binary_input_stream(Stream), Result, !IO) :-
    read_byte_val(input_stream(Stream), ResultCode, Error, Int, !IO),
    Int8 = cast_from_int(Int), % This call cannot throw an exception.
    interpret_result_code1(ResultCode, Error, Int8, Result, !IO).

read_binary_int8_unboxed(binary_input_stream(Stream), Result, Int8, !IO) :-
    read_byte_val(input_stream(Stream), ResultCode, Error, Int, !IO),
    Int8 = cast_from_int(Int),
    interpret_result_code0(ResultCode, Error, Result, !IO).

read_binary_uint8(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint8(Stream, Result, !IO).

read_binary_uint8(binary_input_stream(Stream), Result, !IO) :-
    read_byte_val(input_stream(Stream), ResultCode, Error, Int, !IO),
    UInt8 = cast_from_int(Int), % This call cannot throw an exception.
    interpret_result_code1(ResultCode, Error, UInt8, Result, !IO).

read_binary_uint8_unboxed(binary_input_stream(Stream), Result, UInt8, !IO) :-
    read_byte_val(input_stream(Stream), ResultCode, Error, Int, !IO),
    UInt8 = cast_from_int(Int),
    interpret_result_code0(ResultCode, Error, Result, !IO).

%---------------------%

putback_byte(Int, !IO) :-
    binary_input_stream(Stream, !IO),
    putback_byte(Stream, Int, !IO).

putback_byte(Stream, Int, !IO) :-
    UInt8 = uint8.cast_from_int(Int /\ 0xff),
    putback_uint8(Stream, UInt8, !IO).

%---------------------%

putback_int8(Int8, !IO) :-
    binary_input_stream(Stream, !IO),
    putback_int8(Stream, Int8, !IO).

putback_int8(binary_input_stream(Stream), Int8, !IO) :-
    UInt8 = uint8.cast_from_int8(Int8),
    putback_uint8_2(Stream, UInt8, Ok, !IO),
    (
        Ok = yes
    ;
        Ok = no,
        throw(io_error_string("failed to put back int8"))
    ).

%---------------------%

putback_uint8(UInt8, !IO) :-
    binary_input_stream(Stream, !IO),
    putback_uint8(Stream, UInt8, !IO).

putback_uint8(binary_input_stream(Stream), UInt8, !IO) :-
    putback_uint8_2(Stream, UInt8, Ok, !IO),
    (
        Ok = yes
    ;
        Ok = no,
        throw(io_error_string("failed to put back uint8"))
    ).

%---------------------%

read_binary_int16(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int16(Stream, Result, !IO).

read_binary_int16(Stream, Result, !IO) :-
    ( if native_byte_order_is_big_endian then
        read_binary_int16_be(Stream, Result, !IO)
    else
        read_binary_int16_le(Stream, Result, !IO)
    ).

%---------------------%

read_binary_int16_le(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int16_le(Stream, Result, !IO).

read_binary_int16_le(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint16(Stream, little_endian, ResultCode, Error,
        IncompleteBytes, UInt16, !IO),
    Int16 = cast_from_uint16(UInt16), % This call cannot throw an exception.
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Int16, Result, !IO).

%---------------------%

read_binary_int16_be(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int16_be(Stream, Result, !IO).

read_binary_int16_be(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint16(Stream, big_endian, ResultCode, Error,
        IncompleteBytes, UInt16, !IO),
    Int16 = cast_from_uint16(UInt16), % This call cannot throw an exception.
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Int16, Result, !IO).

%---------------------%

read_binary_uint16(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint16(Stream, Result, !IO).

read_binary_uint16(Stream, Result, !IO) :-
    ( if native_byte_order_is_big_endian then
        read_binary_uint16_be(Stream, Result, !IO)
    else
        read_binary_uint16_le(Stream, Result, !IO)
    ).

%---------------------%

read_binary_uint16_le(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint16_le(Stream, Result, !IO).

read_binary_uint16_le(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint16(Stream, little_endian, ResultCode, Error,
        IncompleteBytes, UInt16, !IO),
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        UInt16, Result, !IO).

%---------------------%

read_binary_uint16_be(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint16_be(Stream, Result, !IO).

read_binary_uint16_be(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint16(Stream, big_endian, ResultCode, Error,
        IncompleteBytes, UInt16, !IO),
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        UInt16, Result, !IO).

%---------------------%

read_binary_int32(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int32(Stream, Result, !IO).

read_binary_int32(Stream, Result, !IO) :-
    ( if native_byte_order_is_big_endian then
        read_binary_int32_be(Stream, Result, !IO)
    else
        read_binary_int32_le(Stream, Result, !IO)
    ).

%---------------------%

read_binary_int32_le(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int32_le(Stream, Result, !IO).

read_binary_int32_le(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint32(Stream, little_endian, ResultCode, Error,
        IncompleteBytes, UInt32, !IO),
    Int32 = cast_from_uint32(UInt32), % This call cannot throw an exception.
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Int32, Result, !IO).

%---------------------%

read_binary_int32_be(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int32_be(Stream, Result, !IO).

read_binary_int32_be(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint32(Stream, big_endian, ResultCode, Error,
        IncompleteBytes, UInt32, !IO),
    Int32 = cast_from_uint32(UInt32), % This call cannot throw an exception.
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Int32, Result, !IO).

%---------------------%

read_binary_uint32(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint32(Stream, Result, !IO).

read_binary_uint32(Stream, Result, !IO) :-
    ( if native_byte_order_is_big_endian then
        read_binary_uint32_be(Stream, Result, !IO)
    else
        read_binary_uint32_le(Stream, Result, !IO)
    ).

%---------------------%

read_binary_uint32_le(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint32_le(Stream, Result, !IO).

read_binary_uint32_le(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint32(Stream, little_endian, ResultCode, Error,
        IncompleteBytes, UInt32, !IO),
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        UInt32, Result, !IO).

%---------------------%

read_binary_uint32_be(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint32_be(Stream, Result, !IO).

read_binary_uint32_be(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint32(Stream, big_endian, ResultCode, Error,
        IncompleteBytes, UInt32, !IO),
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        UInt32, Result, !IO).

%---------------------%

read_binary_int64(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int64(Stream, Result, !IO).

read_binary_int64(Stream, Result, !IO) :-
    ( if native_byte_order_is_big_endian then
        read_binary_int64_be(Stream, Result, !IO)
    else
        read_binary_int64_le(Stream, Result, !IO)
    ).

%---------------------%

read_binary_int64_le(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int64_le(Stream, Result, !IO).

read_binary_int64_le(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint64(Stream, little_endian, ResultCode, Error,
        IncompleteBytes, UInt64, !IO),
    Int64 = cast_from_uint64(UInt64), % This call cannot throw an exception.
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Int64, Result, !IO).

%---------------------%

read_binary_int64_be(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_int64_be(Stream, Result, !IO).

read_binary_int64_be(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint64(Stream, big_endian, ResultCode, Error,
        IncompleteBytes, UInt64, !IO),
    Int64 = cast_from_uint64(UInt64), % This call cannot throw an exception.
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Int64, Result, !IO).

%---------------------%

read_binary_uint64(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint64(Stream, Result, !IO).

read_binary_uint64(Stream, Result, !IO) :-
    ( if native_byte_order_is_big_endian then
        read_binary_uint64_be(Stream, Result, !IO)
    else
        read_binary_uint64_le(Stream, Result, !IO)
    ).

%---------------------%

read_binary_uint64_le(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint64_le(Stream, Result, !IO).

read_binary_uint64_le(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint64(Stream, little_endian, ResultCode, Error,
        IncompleteBytes, UInt64, !IO),
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        UInt64, Result, !IO).

%---------------------%

read_binary_uint64_be(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_uint64_be(Stream, Result, !IO).

read_binary_uint64_be(binary_input_stream(Stream), Result, !IO) :-
    do_read_binary_uint64(Stream, big_endian, ResultCode, Error,
        IncompleteBytes, UInt64, !IO),
    interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        UInt64, Result, !IO).

%---------------------%

:- type byte_order
    --->    big_endian
    ;       little_endian.

:- pragma foreign_export_enum("C", byte_order/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("Java", byte_order/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("C#", byte_order/0,
    [prefix("ML_"), uppercase]).

:- pred native_byte_order_is_big_endian is semidet.

:- pragma foreign_proc("C",
    native_byte_order_is_big_endian,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    #if defined(MR_BIG_ENDIAN)
        SUCCESS_INDICATOR = MR_TRUE;
    #else
        SUCCESS_INDICATOR = MR_FALSE;
    #endif
").

:- pragma foreign_proc("C#",
    native_byte_order_is_big_endian,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = !(System.BitConverter.IsLittleEndian);
").

:- pragma foreign_proc("Java",
    native_byte_order_is_big_endian,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR =
        (java.nio.ByteOrder.nativeOrder() == java.nio.ByteOrder.BIG_ENDIAN);
").

%---------------------------------------------------------------------------%
%
% Writing values of primitive types.
%

write_char(Character, !IO) :-
    output_stream(Stream, !IO),
    write_char(Stream, Character, !IO).

write_char(output_stream(Stream), Character, !IO) :-
    do_write_char(Stream, Character, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_int(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int(Stream, Val, !IO).

write_int(output_stream(Stream), Val, !IO) :-
    do_write_int(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_uint(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint(Stream, Val, !IO).

write_uint(output_stream(Stream), Val, !IO) :-
    do_write_uint(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_int8(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int8(Stream, Val, !IO).

write_int8(output_stream(Stream), Val, !IO) :-
    do_write_int8(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_uint8(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint8(Stream, Val, !IO).

write_uint8(output_stream(Stream), Val, !IO) :-
    do_write_uint8(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_int16(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int16(Stream, Val, !IO).

write_int16(output_stream(Stream), Val, !IO) :-
    do_write_int16(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_uint16(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint16(Stream, Val, !IO).

write_uint16(output_stream(Stream), Val, !IO) :-
    do_write_uint16(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_int32(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int32(Stream, Val, !IO).

write_int32(output_stream(Stream), Val, !IO) :-
    do_write_int32(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_uint32(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint32(Stream, Val, !IO).

write_uint32(output_stream(Stream), Val, !IO) :-
    do_write_uint32(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_int64(Val, !IO) :-
    output_stream(Stream, !IO),
    write_int64(Stream, Val, !IO).

write_int64(output_stream(Stream), Val, !IO) :-
    do_write_int64(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_uint64(Val, !IO) :-
    output_stream(Stream, !IO),
    write_uint64(Stream, Val, !IO).

write_uint64(output_stream(Stream), Val, !IO) :-
    do_write_uint64(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_float(Val, !IO) :-
    output_stream(Stream, !IO),
    write_float(Stream, Val, !IO).

write_float(output_stream(Stream), Val, !IO) :-
    do_write_float(Stream, Val, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_string(Message, !IO) :-
    output_stream(Stream, !IO),
    write_string(Stream, Message, !IO).

write_string(output_stream(Stream), Message, !IO) :-
    do_write_string(Stream, Message, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

nl(!IO) :-
    write_char('\n', !IO).

nl(Stream, !IO) :-
    write_char(Stream, '\n', !IO).

%---------------------------------------------------------------------------%

write_byte(Byte, !IO) :-
    binary_output_stream(Stream, !IO),
    write_byte(Stream, Byte, !IO).

write_byte(binary_output_stream(Stream), Byte, !IO) :-
    do_write_byte(Stream, Byte, Error, !IO),
    throw_on_output_error(Error, !IO).

write_binary_int8(Int8, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_int8(Stream, Int8, !IO).

write_binary_int8(binary_output_stream(Stream), Int8, !IO) :-
    UInt8 = uint8.cast_from_int8(Int8),
    Int = uint8.to_int(UInt8),
    do_write_byte(Stream, Int, Error, !IO),
    throw_on_output_error(Error, !IO).

write_binary_uint8(UInt8, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint8(Stream, UInt8, !IO).

write_binary_uint8(binary_output_stream(Stream), UInt8, !IO) :-
    Int = uint8.to_int(UInt8),
    do_write_byte(Stream, Int, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int16(Int16, !IO) :-
    UInt16 = uint16.cast_from_int16(Int16),
    write_binary_uint16(UInt16, !IO).

write_binary_int16(Stream, Int16, !IO) :-
    UInt16 = uint16.cast_from_int16(Int16),
    write_binary_uint16(Stream, UInt16, !IO).

write_binary_uint16(UInt16, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint16(Stream, UInt16, !IO).

write_binary_uint16(binary_output_stream(Stream), UInt16, !IO) :-
    do_write_binary_uint16(Stream, UInt16, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int16_le(Int16, !IO) :-
    UInt16 = uint16.cast_from_int16(Int16),
    write_binary_uint16_le(UInt16, !IO).

write_binary_int16_le(Stream, Int16, !IO) :-
    UInt16 = uint16.cast_from_int16(Int16),
    write_binary_uint16_le(Stream, UInt16, !IO).

write_binary_uint16_le(UInt16, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint16_le(Stream, UInt16, !IO).

write_binary_uint16_le(binary_output_stream(Stream), UInt16, !IO) :-
    do_write_binary_uint16_le(Stream, UInt16, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int16_be(Int16, !IO) :-
    UInt16 = uint16.cast_from_int16(Int16),
    write_binary_uint16_be(UInt16, !IO).

write_binary_int16_be(Stream, Int16, !IO) :-
    UInt16 = uint16.cast_from_int16(Int16),
    write_binary_uint16_be(Stream, UInt16, !IO).

write_binary_uint16_be(UInt16, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint16_be(Stream, UInt16, !IO).

write_binary_uint16_be(binary_output_stream(Stream), UInt16, !IO) :-
    do_write_binary_uint16_be(Stream, UInt16, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int32(Int32, !IO) :-
    UInt32 = uint32.cast_from_int32(Int32),
    write_binary_uint32(UInt32, !IO).

write_binary_int32(Stream, Int32, !IO) :-
    UInt32 = uint32.cast_from_int32(Int32),
    write_binary_uint32(Stream, UInt32, !IO).

write_binary_uint32(UInt32, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint32(Stream, UInt32, !IO).

write_binary_uint32(binary_output_stream(Stream), UInt32, !IO) :-
    do_write_binary_uint32(Stream, UInt32, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int32_le(Int32, !IO) :-
    UInt32 = uint32.cast_from_int32(Int32),
    write_binary_uint32_le(UInt32, !IO).

write_binary_int32_le(Stream, Int32, !IO) :-
    UInt32 = uint32.cast_from_int32(Int32),
    write_binary_uint32_le(Stream, UInt32, !IO).

write_binary_uint32_le(UInt32, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint32_le(Stream, UInt32, !IO).

write_binary_uint32_le(binary_output_stream(Stream), UInt32, !IO) :-
    do_write_binary_uint32_le(Stream, UInt32, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int32_be(Int32, !IO) :-
    UInt32 = uint32.cast_from_int32(Int32),
    write_binary_uint32_be(UInt32, !IO).

write_binary_int32_be(Stream, Int32, !IO) :-
    UInt32 = uint32.cast_from_int32(Int32),
    write_binary_uint32_be(Stream, UInt32, !IO).

write_binary_uint32_be(UInt32, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint32_be(Stream, UInt32, !IO).

write_binary_uint32_be(binary_output_stream(Stream), UInt32, !IO) :-
    do_write_binary_uint32_be(Stream, UInt32, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int64(Int64, !IO) :-
    UInt64 = uint64.cast_from_int64(Int64),
    write_binary_uint64(UInt64, !IO).

write_binary_int64(Stream, Int64, !IO) :-
    UInt64 = uint64.cast_from_int64(Int64),
    write_binary_uint64(Stream, UInt64, !IO).

write_binary_uint64(UInt64, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint64(Stream, UInt64, !IO).

write_binary_uint64(binary_output_stream(Stream), UInt64, !IO) :-
    do_write_binary_uint64(Stream, UInt64, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int64_le(Int64, !IO) :-
    UInt64 = uint64.cast_from_int64(Int64),
    write_binary_uint64_le(UInt64, !IO).

write_binary_int64_le(Stream, Int64, !IO) :-
    UInt64 = uint64.cast_from_int64(Int64),
    write_binary_uint64_le(Stream, UInt64, !IO).

write_binary_uint64_le(UInt64, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint64_le(Stream, UInt64, !IO).

write_binary_uint64_le(binary_output_stream(Stream), UInt64, !IO) :-
    do_write_binary_uint64_le(Stream, UInt64, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_int64_be(Int64, !IO) :-
    UInt64 = uint64.cast_from_int64(Int64),
    write_binary_uint64_be(UInt64, !IO).

write_binary_int64_be(Stream, Int64, !IO) :-
    UInt64 = uint64.cast_from_int64(Int64),
    write_binary_uint64_be(Stream, UInt64, !IO).

write_binary_uint64_be(UInt64, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_uint64_be(Stream, UInt64, !IO).

write_binary_uint64_be(binary_output_stream(Stream), UInt64, !IO) :-
    do_write_binary_uint64_be(Stream, UInt64, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

write_binary_string_utf8(String, !IO) :-
    binary_output_stream(Stream, !IO),
    write_binary_string_utf8(Stream, String, !IO).

write_binary_string_utf8(binary_output_stream(Stream), String, !IO) :-
    do_write_binary_string_utf8(Stream, String, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------------------------------------------------------------%
%
% Text input predicates.
%

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

read_line(Result, !IO) :-
    input_stream(Stream, !IO),
    read_line(Stream, Result, !IO).

read_line(Stream, Result, !IO) :-
    read_line_2(Stream, ResultCode, Error, Chars, !IO),
    (
        ResultCode = result_code_ok,
        Result = ok(Chars)
    ;
        ResultCode = result_code_eof,
        Result = eof
    ;
        ResultCode = result_code_error,
        make_io_error_from_system_error(Error, "read failed: ", IOError, !IO),
        Result = error(IOError)
    ).

read_line_as_string(Result, !IO) :-
    input_stream(Stream, !IO),
    read_line_as_string(Stream, Result, !IO).

read_line_as_string(input_stream(Stream), Result, !IO) :-
    read_line_as_string_2(Stream, yes, Res, Error, String, !IO),
    (
        Res = rlas_ok,
        Result = ok(String)
    ;
        Res = rlas_eof,
        Result = eof
    ;
        Res = rlas_null_char,
        Result = error(io_error_string("null character in input"))
    ;
        Res = rlas_error,
        make_io_error_from_system_error(Error, "read failed: ", IOError, !IO),
        Result = error(IOError)
    ).

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
% Bitmap input and output predicates.
%

read_bitmap(!Bitmap, BytesRead, Result, !IO) :-
    bitmap.read_bitmap(!Bitmap, BytesRead, Result, !IO).

read_bitmap(Stream, !Bitmap, BytesRead, Result, !IO) :-
    bitmap.read_bitmap(Stream, !Bitmap, BytesRead, Result, !IO).

read_bitmap(StartByte, NumBytes, !Bitmap, BytesRead, Result, !IO) :-
    bitmap.read_bitmap_range(StartByte, NumBytes, !Bitmap,
        BytesRead, Result, !IO).

read_bitmap(Stream, Start, NumBytes, !Bitmap, BytesRead, Result, !IO) :-
    bitmap.read_bitmap_range(Stream, Start, NumBytes, !Bitmap,
        BytesRead, Result, !IO).

%---------------------%

write_bitmap(Bitmap, !IO) :-
    bitmap.write_bitmap(Bitmap, !IO).

write_bitmap(Stream, Bitmap, !IO) :-
    bitmap.write_bitmap(Stream, Bitmap, !IO).

write_bitmap(Bitmap, Start, NumBytes, !IO) :-
    bitmap.write_bitmap_range(Bitmap, Start, NumBytes, !IO).

write_bitmap(Stream, Bitmap, Start, NumBytes, !IO) :-
    bitmap.write_bitmap_range(Stream, Bitmap, Start, NumBytes, !IO).

%---------------------------------------------------------------------------%
%
% Reading values of arbitrary types.
%

read(Result, !IO) :-
    io.input_stream(Stream, !IO),
    mercury_term_parser.read_term(Stream, ReadResult, !IO),
    get_line_number(Stream, LineNumber, !IO),
    process_read_term($pred, ReadResult, LineNumber, Result).

read(Stream, Result, !IO) :-
    mercury_term_parser.read_term(Stream, ReadResult, !IO),
    get_line_number(Stream, LineNumber, !IO),
    process_read_term($pred, ReadResult, LineNumber, Result).

init_posn = posn(1, 0, 0).

read_from_string(FileName, String, Len, Result, !Posn) :-
    mercury_term_parser.read_term_from_substring(FileName, String, Len,
        !Posn, ReadResult),
    !.Posn = posn(LineNumber, _, _),
    process_read_term($pred, ReadResult, LineNumber, Result).

:- pred process_read_term(string::in, read_term::in, int::in,
    io.read_result(T)::out) is det.

process_read_term(PredId, ReadResult, LineNumber, Result) :-
    (
        ReadResult = term(_VarSet, Term),
        ( if term_to_type(Term, Type) then
            Result = ok(Type)
        else
            ( if term_subst.term_is_ground(Term) then
                Msg = "the term read did not have the right type"
            else
                Msg = "the term read was not a ground term"
            ),
            Result = error(PredId ++ ": " ++ Msg, LineNumber)
        )
    ;
        ReadResult = eof,
        Result = eof
    ;
        ReadResult = error(Msg, LN),
        Result = error(Msg, LN)
    ).

%---------------------%

read_binary(Result, !IO) :-
    binary_input_stream(BinaryInputStream, !IO),
    read_binary(BinaryInputStream, Result, !IO).

read_binary(BinaryInputStream, Result, !IO) :-
    % XXX This "cast" will not work for the Java back-end.
    % See the comment at the top of the MR_MercuryFileStruct class definition
    % in io.stream_ops.m.
    BinaryInputStream = binary_input_stream(Stream),
    TextInputStream = input_stream(Stream),
    read_binary_from_text_input_stream(TextInputStream, Result, !IO).

:- pred read_binary_from_text_input_stream(io.text_input_stream::in,
    io.result(T)::out, io::di, io::uo) is det.

read_binary_from_text_input_stream(Stream, Result, !IO) :-
    read(Stream, ReadResult, !IO),
    (
        ReadResult = ok(T),
        % We have read the newline and the trailing full stop.
        % Now skip the newline after the full stop.
        read_char(Stream, NewLineRes, !IO),
        (
            NewLineRes = error(Error),
            Result = error(Error)
        ;
            NewLineRes = ok(NewLineChar),
            ( if NewLineChar = '\n' then
                Result = ok(T)
            else
                Result = error(
                    io_error_string("io.read_binary: missing newline"))
            )
        ;
            NewLineRes = eof,
            Result = error(io_error_string("io.read_binary: missing newline"))
        )
    ;
        ReadResult = eof,
        Result = eof
    ;
        % XXX ERROR: would need to change read_result(T) to return io.error
        ReadResult = error(ErrorMsg, _Line),
        Result = error(io_error_string(ErrorMsg))
    ).

%---------------------------------------------------------------------------%
%
% Writing values of arbitrary types.
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

%---------------------%

    % XXX Only use this for debugging. Strictly speaking, io.print may throw an
    % exception which is not allowed across the C interface.
    %
:- pred print_to_stream(io.stream::in, T::in, io::di, io::uo) is det.
:- pragma foreign_export("C", io.print_to_stream(in, in, di, uo),
    "ML_io_print_to_stream").

print_to_stream(Stream, Term, !IO) :-
    io.print(output_stream(Stream), canonicalize, Term, !IO).

%---------------------%

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
    output_stream(Stream, !IO),
    write_line(Stream, X, !IO).

write_line(Stream, X, !IO) :-
    io.write(Stream, X, !IO),
    io.nl(Stream, !IO).

write_line(Stream, NonCanon, X, !IO) :-
    io.write(Stream, NonCanon, X, !IO),
    io.nl(Stream, !IO).

write_line_cc(X, !IO) :-
    output_stream(Stream, !IO),
    write_line_cc(Stream, X, !IO).

write_line_cc(Stream, X, !IO) :-
    io.write_cc(Stream, X, !IO),
    io.nl(Stream, !IO).

%---------------------%

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

%---------------------------------------------------------------------------%
%
% Formatted output.
%

format(FormatString, Arguments, !IO) :-
    output_stream(Stream, !IO),
    disable_warning [unknown_format_calls] (
        io.format(Stream, FormatString, Arguments, !IO)
    ).

format(Stream, FormatString, Arguments, !IO) :-
    disable_warning [unknown_format_calls] (
        string.format(FormatString, Arguments, String)
    ),
    write_string(Stream, String, !IO).

%---------------------------------------------------------------------------%
%
% Writing out several values.
%

write_strings(Strings, !IO) :-
    output_stream(Stream, !IO),
    write_strings(Stream, Strings, !IO).

write_strings(_Stream, [], !IO).
write_strings(Stream, [S | Ss], !IO) :-
    write_string(Stream, S, !IO),
    write_strings(Stream, Ss, !IO).

%---------------------%

write_many(Vals, !IO) :-
    output_stream(Stream, !IO),
    write_many(Stream, Vals, !IO).

write_many(_Stream, [], !IO).
write_many(Stream, [Val | Vals], !IO) :-
    (
        Val = c(C),
        write_char(Stream, C, !IO)
    ;
        Val = i(I),
        write_int(Stream, I, !IO)
    ;
        Val = i8(I),
        write_int8(Stream, I, !IO)
    ;
        Val = i16(I),
        write_int16(Stream, I, !IO)
    ;
        Val = i32(I),
        write_int32(Stream, I, !IO)
    ;
        Val = i64(I),
        write_int64(Stream, I, !IO)
    ;
        Val = u(U),
        write_uint(Stream, U, !IO)
    ;
        Val = u8(U),
        write_uint8(Stream, U, !IO)
    ;
        Val = u16(U),
        write_uint16(Stream, U, !IO)
    ;
        Val = u32(U),
        write_uint32(Stream, U, !IO)
    ;
        Val = u64(U),
        write_uint64(Stream, U, !IO)
    ;
        Val = s(S),
        write_string(Stream, S, !IO)
    ;
        Val = f(F),
        write_float(Stream, F, !IO)
    ),
    write_many(Stream, Vals, !IO).

%---------------------%

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

%---------------------%

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
%
% Flushing output to the operating system.
%

flush_output(!IO) :-
    output_stream(Stream, !IO),
    flush_output(Stream, !IO).

flush_output(output_stream(Stream), !IO) :-
    flush_text_output_2(Stream, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------%

flush_binary_output(!IO) :-
    binary_output_stream(Stream, !IO),
    flush_binary_output(Stream, !IO).

flush_binary_output(binary_output_stream(Stream), !IO) :-
    flush_binary_output_2(Stream, Error, !IO),
    throw_on_output_error(Error, !IO).

%---------------------------------------------------------------------------%
%
% Whole file input predicates.
%

read_named_file_as_string(FileName, Result, !IO) :-
    io.open_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(FileStream),
        io.read_file_as_string(FileStream, ReadResult, !IO),
        (
            ReadResult = ok(String),
            Result = ok(String)
        ;
            ReadResult = error(_PartialString, Error),
            Result = error(Error)
        ),
        io.close_input(FileStream, !IO)
    ;
        OpenResult = error(Error),
        Result = error(Error)
    ).

read_named_file_as_lines(FileName, Result, !IO) :-
    io.open_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(FileStream),
        io.read_file_as_string(FileStream, ReadResult, !IO),
        (
            ReadResult = ok(String),
            Lines = split_into_lines(String),
            Result = ok(Lines)
        ;
            ReadResult = error(_PartialString, Error),
            Result = error(Error)
        ),
        io.close_input(FileStream, !IO)
    ;
        OpenResult = error(Error),
        Result = error(Error)
    ).

%---------------------%

read_file(Result, !IO) :-
    input_stream(Stream, !IO),
    read_file(Stream, Result, !IO).

read_file(Stream, Result, !IO) :-
    read_file_chars_acc(Stream, [], Result, !IO).

:- pred read_file_chars_acc(input_stream::in, list(char)::in,
    maybe_partial_res(list(char))::out, io::di, io::uo) is det.

read_file_chars_acc(Stream, RevChars0, Result, !IO) :-
    read_char(Stream, Result0, !IO),
    (
        Result0 = eof,
        Result = ok(list.reverse(RevChars0))
    ;
        Result0 = error(Error),
        Result = error(list.reverse(RevChars0), Error)
    ;
        Result0 = ok(Char),
        RevChars = [Char | RevChars0],
        read_file_chars_acc(Stream, RevChars, Result, !IO)
    ).

%---------------------%

read_file_as_string(Result, !IO) :-
    input_stream(Stream, !IO),
    read_file_as_string(Stream, Result, !IO).

read_file_as_string(input_stream(Stream), Result, !IO) :-
    read_file_as_string_2(Stream, String, _NumCUs, Error, NullCharError, !IO),
    is_error(Error, "read failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(String, IOError)
    ;
        MaybeIOError = no,
        (
            NullCharError = yes,
            Result = error("", io_error_string("null character in input"))
        ;
            NullCharError = no,
            Result = ok(String)
        )
    ).

read_file_as_string_and_num_code_units(Result, !IO) :-
    input_stream(Stream, !IO),
    read_file_as_string_and_num_code_units(Stream, Result, !IO).

read_file_as_string_and_num_code_units(input_stream(Stream), Result, !IO) :-
    read_file_as_string_2(Stream, String, NumCUs, Error, NullCharError, !IO),
    is_error(Error, "read failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error2(String, NumCUs, IOError)
    ;
        MaybeIOError = no,
        (
            NullCharError = yes,
            Result = error2("", 0, io_error_string("null character in input"))
        ;
            NullCharError = no,
            Result = ok2(String, NumCUs)
        )
    ).

%---------------------%

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

%---------------------%

read_binary_file_as_bitmap(Result, !IO) :-
    binary_input_stream(Stream, !IO),
    read_binary_file_as_bitmap(Stream, Result, !IO).

read_binary_file_as_bitmap(Stream, Result, !IO) :-
    read_binary_file_as_bitmap_2(Stream, Result, !IO).

%---------------------------------------------------------------------------%
%
% Processing the contents of a whole file.
%

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

%---------------------%

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

:- type chunk_inner_res0
    --->    cir0_ok
    ;       cir0_error(io.error)
    ;       cir0_more.

:- type chunk_inner_res(T)
    --->    cir_ok(T)
    ;       cir_error(T, io.error)
    ;       cir_more(T).

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
        InnerRes = cir0_ok,
        Res = ok
    ;
        InnerRes = cir0_error(Error),
        Res = error(Error)
    ;
        InnerRes = cir0_more,
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
            Res = cir0_ok
        ;
            ByteResult = error(Error),
            Res = cir0_error(Error)
        )
    else
        Res = cir0_more
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
        InnerRes = cir_ok(T),
        Res = ok(T)
    ;
        InnerRes = cir_error(T, Error),
        Res = error(T, Error)
    ;
        InnerRes = cir_more(T1),
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
            Res = cir_ok(T0)
        ;
            ByteResult = error(Error),
            Res = cir_error(T0, Error)
        )
    else
        Res = cir_more(T0)
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
            binary_input_stream_foldl2_io_maybe_stop_plain(Stream, Pred, T1,
                Res, !IO)
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
        InnerRes = cir_ok(T),
        Res = ok(T)
    ;
        InnerRes = cir_error(T, Error),
        Res = error(T, Error)
    ;
        InnerRes = cir_more(T1),
        binary_input_stream_foldl2_io_maybe_stop_chunk(Stream, Pred, T1,
            Res, !IO)
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
                Res = cir_ok(T1)
            ;
                Continue = yes,
                binary_input_stream_foldl2_io_maybe_stop_inner(Left - 1,
                    Stream, Pred, T1, Res, !IO)
            )
        ;
            ByteResult = eof,
            Res = cir_ok(T0)
        ;
            ByteResult = error(Error),
            Res = cir_error(T0, Error)
        )
    else
        Res = cir_more(T0)
    ).

:- pred should_reduce_stack_usage(bool::out) is det.

% For non-C backends.
should_reduce_stack_usage(yes).

:- pragma foreign_proc("C",
    should_reduce_stack_usage(ShouldReduce::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
#ifdef  MR_EXEC_TRACE
    ShouldReduce = MR_YES;
#else
    ShouldReduce = MR_NO;
#endif
").

    % Chunk_size gives the maximum number of recursive calls we want to allow
    % in the binary_input_stream_foldl*_inner predicates. Without such a limit,
    % the depth of recursion, which depends on the size of the file they read,
    % will cause exhaustion of the det stack in debug grades, since there is
    % no tail recursion in such grades.
    %
    % With this arrangement, the maximum number of stack frames needed
    % to process a file of size N is N/1000 + 1000, the former being the
    % number of frames of binary_input_stream_foldl*_chunk predicates,
    % the latter being the max number of frames of the *_inner predicates.
    %
:- func chunk_size = int.

chunk_size = 1000.

%---------------------------------------------------------------------------%
%
% File handling predicates.
%

remove_file(FileName, Result, !IO) :-
    io.file.remove_file(FileName, Result, !IO).

remove_file_recursively(FileName, Res, !IO) :-
    io.file.remove_file_recursively(FileName, Res, !IO).

rename_file(OldFileName, NewFileName, Result, !IO) :-
    io.file.rename_file(OldFileName, NewFileName, Result, !IO).

have_symlinks :-
    io.file.have_symlinks.

make_symlink(FileName, LinkFileName, Result, !IO) :-
    io.file.make_symlink(FileName, LinkFileName, Result, !IO).

read_symlink(FileName, Result, !IO) :-
    io.file.read_symlink(FileName, Result, !IO).

check_file_accessibility(FileName, AccessTypes, Result, !IO) :-
    io.file.check_file_accessibility(FileName, AccessTypes, Result, !IO).

file_type(FollowSymLinks, FileName, Result, !IO) :-
    io.file.file_type(FollowSymLinks, FileName, Result, !IO).

file_modification_time(File, Result, !IO) :-
    io.file.file_modification_time(File, Result, !IO).

%---------------------------------------------------------------------------%
%
% Predicates for handling temporary files.
%

make_temp_file(Result, !IO) :-
    io.file.make_temp_file(Result, !IO).

make_temp_file(Dir, Prefix, Suffix, Result, !IO) :-
    io.file.make_temp_file(Dir, Prefix, Suffix, Result, !IO).

make_temp_directory(Result, !IO) :-
    io.file.make_temp_directory(Result, !IO).

make_temp_directory(Dir, Prefix, Suffix, Result, !IO) :-
    io.file.make_temp_directory(Dir, Prefix, Suffix, Result, !IO).

have_make_temp_directory :-
    io.file.have_make_temp_directory.

get_temp_directory(Dir, !IO) :-
    io.file.get_temp_directory(Dir, !IO).

%---------------------------------------------------------------------------%
%
% Global state predicates.
%

:- pragma foreign_proc("C",
    progname(DefaultProgname::in, PrognameOut::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate],
"
    if ((MR_progname != NULL) && MR_progname_is_known) {
        MR_make_aligned_string(PrognameOut, MR_progname);
    } else {
        PrognameOut = DefaultProgname;
    }
").

:- pragma foreign_proc("Java",
    progname(Default::in, PrognameOut::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    PrognameOut = jmercury.runtime.JavaInternal.progname;
    if (PrognameOut == null) {
        PrognameOut = Default;
    }
").

    % Fallback implementation.
progname(DefaultProgName, ProgName, !IO) :-
    ProgName = DefaultProgName.

progname_base(DefaultName, PrognameBase, !IO) :-
    progname(DefaultName, Progname, !IO),
    PrognameBase = dir.det_basename(Progname).

%---------------------%

:- pragma foreign_proc("C",
    command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate,
        no_sharing],
    % no_sharing is okay because the string elements can't be reused.
"{
    int i;

    // Convert mercury_argv from a vector to a list.
    i = mercury_argc;
    Args = MR_list_empty_msg(MR_ALLOC_ID);
    while (--i >= 0) {
        Args = MR_string_list_cons_msg((MR_Word) mercury_argv[i], Args,
            MR_ALLOC_ID);
    }
}").

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

:- pragma foreign_proc("Java",
    command_line_arguments(Args::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    String[] arg_vector = jmercury.runtime.JavaInternal.args;
    Args = list.empty_list();
    // The argument vector may not be set if Mercury is being used
    // as a library by a native Java application.
    if (arg_vector != null) {
        // arg_vector does not include the executable name.
        for (int i = arg_vector.length - 1; i >= 0; --i) {
            Args = list.cons(arg_vector[i], Args);
        }
    }
").

%---------------------%

:- pragma foreign_proc("C",
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ExitStatus = mercury_exit_status;
").

:- pragma foreign_proc("C#",
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ExitStatus = System.Environment.ExitCode;
").

:- pragma foreign_proc("Java",
    get_exit_status(ExitStatus::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    ExitStatus = jmercury.runtime.JavaInternal.exit_status;
").

%---------------------%

:- pragma foreign_proc("C",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    mercury_exit_status = (int) ExitStatus;
").

:- pragma foreign_proc("C#",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    System.Environment.ExitCode = ExitStatus;
").

:- pragma foreign_proc("Java",
    set_exit_status(ExitStatus::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    jmercury.runtime.JavaInternal.exit_status = ExitStatus;
").

%---------------------%

get_environment_var(Var, OptValue, !IO) :-
    io.environment.get_environment_var(Var, OptValue, !IO).

set_environment_var(Var, Value, Res, !IO) :-
    io.environment.set_environment_var(Var, Value, Res, !IO).

set_environment_var(Var, Value, !IO) :-
    io.environment.set_environment_var(Var, Value, !IO).

have_set_environment_var :-
    io.environment.have_set_environment_var.

get_environment_var_map(EnvVarMap, !IO) :-
    io.environment.get_environment_var_map(EnvVarMap, !IO).

%---------------------------------------------------------------------------%
%
% System access predicates.
%

call_system(Command, Result, !IO) :-
    io.call_system.call_system(Command, Result, !IO).

call_system_return_signal(Command, Result, !IO) :-
    io.call_system.call_system_return_signal(Command, Result, !IO).

%---------------------------------------------------------------------------%
%
% Managing the globals structure that Mercury attaches to the I/O state.
%

get_globals(Globals, !IO) :-
    lock_globals(!IO),
    unsafe_get_globals(Globals, !IO),
    unlock_globals(!IO).

set_globals(Globals, !IO) :-
    lock_globals(!IO),
    unsafe_set_globals(Globals, !IO),
    unlock_globals(!IO).

update_globals(UpdatePred, !IO) :-
    promise_pure (
        lock_globals(!IO),
        unsafe_get_globals(Globals0, !IO),
        promise_equivalent_solutions [!:IO] (
            Update =
                ( pred(G::out) is det :-
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
unlock_globals :-
    impure impure_true.

%---------------------%

    % NOTE: io.unsafe_{get, set}_globals/3 are marked as thread_safe so that
    % calling them to does not acquire the global lock. Since calls to these
    % predicates should be surrounded by calls to io.{lock, unlock}_globals/2,
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

:- pragma foreign_proc("C#",
    unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Globals = io.ML_io_user_globals;
").

:- pragma foreign_proc("Java",
    unsafe_get_globals(Globals::out, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Globals = io.ML_io_user_globals;
").

%---------------------%

:- pred unsafe_set_globals(univ::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unsafe_set_globals(Globals::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    // XXX need to globalize the memory.
    ML_io_user_globals = Globals;
").

:- pragma foreign_proc("C#",
    unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    io.ML_io_user_globals = Globals;
").

:- pragma foreign_proc("Java",
    unsafe_set_globals(Globals::in, _IOState0::di, _IOState::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    io.ML_io_user_globals = Globals;
").

%---------------------------------------------------------------------------%
%
% Predicates that report statistics about the current program execution.
%

report_stats(Stream, Selector, !IO) :-
    benchmarking.report_stats(Stream, Selector, !IO).

report_stats(Selector, !IO) :-
    benchmarking.report_stats(Selector, !IO).

%---------------------%

report_standard_stats(output_stream(Stream), !IO) :-
    benchmarking.report_standard_stats(output_stream(Stream), !IO).

report_standard_stats(!IO) :-
    benchmarking.report_standard_stats(!IO).

%---------------------%

report_full_memory_stats(output_stream(Stream), !IO) :-
    benchmarking.report_full_memory_stats(output_stream(Stream), !IO).

report_full_memory_stats(!IO) :-
    benchmarking.report_full_memory_stats(!IO).

%---------------------%

report_tabling_statistics(output_stream(Stream), !IO) :-
    benchmarking.report_tabling_statistics(output_stream(Stream), !IO).

report_tabling_statistics(!IO) :-
    benchmarking.report_tabling_statistics(!IO).

%---------------------------------------------------------------------------%
%
% Interpreting I/O error messages.
%

:- type io.error
    --->    io_error_string(string)
    ;       io_error_errno(string, system_error)
    ;       io_error_win32(string, system_error)
    ;       io_error_exception_object(string, system_error).

make_io_error(Error) = io_error_string(Error).

make_io_error_from_system_error(Error, Prefix, IOError, !IO) :-
    io.error_util.make_io_error_from_system_error_impl(Error, Prefix, IOError,
        !IO).

make_io_error_from_windows_error(Error, Prefix, IOError, !IO) :-
    io.error_util.make_io_error_from_windows_error_impl(Error, Prefix, IOError,
        !IO).

%---------------------%

error_message(Error) = Msg :-
    error_message(Error, Msg).

error_message(Error, Msg) :-
    ( Error = io_error_string(Msg)
    ; Error = io_error_errno(Msg, _)
    ; Error = io_error_win32(Msg, _)
    ; Error = io_error_exception_object(Msg, _)
    ).

%---------------------%

get_system_error(Error, SystemError) :-
    require_complete_switch [Error]
    ( Error = io_error_string(_), fail
    ; Error = io_error_errno(_, SystemError)
    ; Error = io_error_win32(_, SystemError)
    ; Error = io_error_exception_object(_, SystemError)
    ).

get_errno_error(Error, Errno) :-
    Error = io_error_errno(_, Errno).

get_windows_error(Error, ErrorCode) :-
    Error = io_error_win32(_, ErrorCode).

get_exception_object_error(Error, Exception) :-
    Error = io_error_exception_object(_, Exception).

%---------------------%

get_system_error_name(Error, Name) :-
    require_complete_switch [Error]
    (
        Error = io_error_string(_),
        fail
    ;
        Error = io_error_errno(_, Errno),
        system_error_errno_name(Errno, Name)
    ;
        Error = io_error_win32(_, ErrorCode),
        system_error_win32_error_name(ErrorCode, Name)
    ;
        Error = io_error_exception_object(_, Exception),
        system_error_exception_name(Exception, Name)
    ).

%---------------------%

:- pred system_error_errno_name(io.system_error::in, string::out) is det.

:- pragma foreign_proc("C",
    system_error_errno_name(Errno::in, Name::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    const char *str = MR_errno_name(Errno);
    if (str != NULL) {
        Name = (MR_String) str;
    } else {
        Name = MR_make_string(MR_ALLOC_ID, ""errno %d"", Errno);
    }
").

system_error_errno_name(_, _) :-
    error("io.system_error_errno_name: inapplicable back-end").

%---------------------%

:- pred system_error_win32_error_name(io.system_error::in, string::out) is det.

:- pragma foreign_proc("C",
    system_error_win32_error_name(ErrorCode::in, Name::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
#ifdef MR_WIN32
    const char *suffix = MR_win32_error_name(ErrorCode);
    if (suffix != NULL) {
        const char prefix[6] = ""ERROR_"";
        size_t prefix_len;
        size_t suffix_len;

        prefix_len = sizeof(prefix);
        suffix_len = strlen(suffix);
        MR_allocate_aligned_string_msg(Name, prefix_len + suffix_len,
            MR_ALLOC_ID);
        MR_memcpy(Name, prefix, prefix_len);
        MR_memcpy(Name + prefix_len, suffix, suffix_len + 1); // include NUL
    } else {
        Name = MR_make_string(MR_ALLOC_ID, ""System error 0x%X"", ErrorCode);
    }
#else
    MR_fatal_error(""io.system_error_win32_error_name: not on Windows"");
#endif
").

system_error_win32_error_name(_, _) :-
    error("io.system_error_win32_error_name: inapplicable back-end").

%---------------------%

:- pred system_error_exception_name(io.system_error::in, string::out) is det.

:- pragma foreign_proc("C#",
    system_error_exception_name(Exception::in, Name::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    if (Exception == null) {
        Name = ""null"";
    } else {
        Name = Exception.GetType().FullName;
    }
").
:- pragma foreign_proc("Java",
    system_error_exception_name(Exception::in, Name::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    if (Exception == null) {
        Name = ""null"";
    } else {
        Name = Exception.getClass().getName();
    }
").

system_error_exception_name(_, _) :-
    error("io.system_error_exception_name: inapplicable back-end").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% For use by library.m.
%

init_state(!IO) :-
    init_std_streams(!IO),
    init_current_streams(!IO),
    io.gc_init(type_of(StreamDb), type_of(Globals), !IO),
    map.init(StreamDb),
    type_to_univ("<globals>", Globals),
    io.stream_db.set_stream_db(StreamDb, !IO),
    io.set_globals(Globals, !IO),
    io.insert_std_stream_names(!IO).

:- pred init_std_streams(io::di, io::uo) is det.

init_std_streams(!IO).

:- pred init_current_streams(io::di, io::uo) is det.

init_current_streams(!IO) :-
    % In C grades the "current" streams are thread-local values, so can only be
    % set after the MR_Context has been initialised for the initial thread.
    set_input_stream(stdin_stream, _, !IO),
    set_output_stream(stdout_stream, _, !IO),
    stdin_binary_stream(StdinBinary, !IO),
    stdout_binary_stream(StdoutBinary, !IO),
    set_binary_input_stream(StdinBinary, _, !IO),
    set_binary_output_stream(StdoutBinary, _, !IO).

:- pragma foreign_proc("C#",
    init_current_streams(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // Do nothing in the C# grade -- setting the current streams is handled
    // during class initialization.
").

:- pragma foreign_proc("Java",
    init_current_streams(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // Do nothing in the Java grade -- setting the current streams is handled
    // during class initialization.
").

:- pred gc_init(type_desc::in, type_desc::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    gc_init(StreamDbType::in, UserGlobalsType::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    // For Windows DLLs, we need to call GC_INIT() from each DLL.
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

    % Currently no finalization needed...
    % (Perhaps we should close all open Mercury files?
    % That will happen on process exit anyway, so currently we don't bother.)
finalize_state(!IO).

%---------------------------------------------------------------------------%
%
% For use by dir.m.
%

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

have_win32 :-
    semidet_fail.

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

have_cygwin :-
    semidet_fail.

:- pragma foreign_proc("C#",
    have_dotnet,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

have_dotnet :-
    semidet_fail.

%---------------------------------------------------------------------------%

:- type file_id
    --->    file_id.

:- pragma foreign_type("C", file_id, "ML_File_Id")
    where comparison is compare_file_id.

:- pragma foreign_decl("C", "
#ifdef MR_WIN32
  #include <wchar.h>    // for dev_t, ino_t
#endif

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
:- pragma consider_used(pred(compare_file_id/3)).

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

    // For compilers other than GCC, glibc defines dev_t as struct (dev_t
    // is 64 bits, and other compilers may not have a 64 bit arithmetic type).
    // XXX This code assumes that dev_t and ino_t do not include padding bits.
    // In practice, that should be OK.
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

compare_file_id_2(_, _, _) :-
    unexpected($pred, "File IDs are not supported by Java").

file_id(FileName, Result, !IO) :-
    file_id_2(FileName, FileId, Error, !IO),
    is_error(Error, "cannot get file id: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(FileId)
    ).

:- pred file_id_2(string::in, file_id::out, system_error::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    file_id_2(FileName::in, FileId::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    // Win32 returns junk in the st_ino field of `struct stat'.
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

%---------------------------------------------------------------------------%
%
% For use by bitmap.m.
%

input_stream_get_stream(input_stream(Stream)) = Stream.
output_stream_get_stream(output_stream(Stream)) = Stream.
binary_input_stream_get_stream(binary_input_stream(Stream)) = Stream.
binary_output_stream_get_stream(binary_output_stream(Stream)) = Stream.

%---------------------------------------------------------------------------%
%
% Predicates to temporarily change the input/output stream.
% XXX We should not need these if we passed streams explicitly everywhere.
%

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

:- pred restore_output_stream(pred(io, io), output_stream, io.res, io, io).
:- mode restore_output_stream(pred(di, uo) is det, in, out, di, uo) is det.
:- mode restore_output_stream(pred(di, uo) is cc_multi, in, out, di, uo)
    is cc_multi.
:- pragma no_determinism_warning(pred(restore_output_stream/5)).

restore_output_stream(_DummyPred, Stream, ok, !IO) :-
    set_output_stream(Stream, _OldStream, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Instances of the stream typeclasses.
%

:- instance stream.error(error) where [
    func(stream.error_message/1) is io.error_message
].

%---------------------------------------------------------------------------%
%
% Text input streams.
%

:- instance stream.stream(text_input_stream, io) where [
    pred(name/4) is input_stream_name
].

:- instance stream.input(text_input_stream, io) where [].

:- instance stream.line_oriented(text_input_stream, io) where
[
    pred(get_line/4) is io.get_line_number,
    pred(set_line/4) is io.set_line_number
].

%---------------------%

:- instance stream.reader(text_input_stream, char, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_char(Stream, Result0, !IO),
        Result = io.result1_to_stream_result1(Result0)
    )
].

:- instance stream.reader(text_input_stream, line, io, io.error)
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

:- instance stream.reader(text_input_stream, text_file, io, io.error)
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

%---------------------%

:- instance stream.unboxed_reader(text_input_stream, char, io, io.error)
    where
[
    ( unboxed_get(Stream, Result, Char, !IO) :-
        read_char_unboxed(Stream, Result0, Char, !IO),
        Result = io.result0_to_stream_result0(Result0)
    )
].

%---------------------%

:- instance stream.putback(text_input_stream, char, io, io.error) where
[
    pred(unget/4) is putback_char
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

:- instance stream.seekable(binary_input_stream, io)
    where
[
    ( seek(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        seek_binary_input(Stream, Whence, OffSet, !IO)
    ),
    ( seek64(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        seek_binary_input64(Stream, Whence, OffSet, !IO)
    )
].

%---------------------%

:- instance stream.reader(binary_input_stream, int, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_byte(Stream, Result0, !IO),
        Result = result1_to_stream_result1(Result0)
    )
].

:- instance stream.reader(binary_input_stream, int8, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_binary_int8(Stream, Result0, !IO),
        Result = result1_to_stream_result1(Result0)
    )
].

:- instance stream.reader(binary_input_stream, uint8, io, io.error)
    where
[
    ( get(Stream, Result, !IO) :-
        read_binary_uint8(Stream, Result0, !IO),
        Result = result1_to_stream_result1(Result0)
    )
].

%---------------------%

:- instance stream.unboxed_reader(binary_input_stream, int8, io, io.error)
    where
[
    ( unboxed_get(Stream, Result, Int8, !IO) :-
        read_binary_int8_unboxed(Stream, Result0, Int8, !IO),
        Result = io.result0_to_stream_result0(Result0)
    )
].

:- instance stream.unboxed_reader(binary_input_stream, uint8, io, io.error)
    where
[
    ( unboxed_get(Stream, Result, UInt8, !IO) :-
        read_binary_uint8_unboxed(Stream, Result0, UInt8, !IO),
        Result = io.result0_to_stream_result0(Result0)
    )
].

%---------------------%

:- instance stream.putback(binary_input_stream, int, io, io.error)
    where
[
    pred(unget/4) is putback_byte
].

:- instance stream.putback(binary_input_stream, int8, io, io.error)
    where
[
    pred(unget/4) is putback_int8
].

:- instance stream.putback(binary_input_stream, uint8, io, io.error)
    where
[
    pred(unget/4) is putback_uint8
].

%---------------------------------------------------------------------------%
%
% Text output streams.
%

:- instance stream.stream(text_output_stream, io) where [
    pred(name/4) is output_stream_name
].

:- instance stream.output(text_output_stream, io) where [
    pred(flush/3) is flush_output
].

:- instance stream.line_oriented(text_output_stream, io) where
[
    pred(get_line/4) is get_output_line_number,
    pred(set_line/4) is set_output_line_number
].

%---------------------%

:- instance stream.writer(text_output_stream, char, io)
    where
[
    pred(put/4) is write_char
].

:- instance stream.writer(text_output_stream, float, io)
    where
[
    pred(put/4) is write_float
].

:- instance stream.writer(text_output_stream, int, io)
    where
[
    pred(put/4) is write_int
].

:- instance stream.writer(text_output_stream, int8, io)
    where
[
    pred(put/4) is write_int8
].

:- instance stream.writer(text_output_stream, int16, io)
    where
[
    pred(put/4) is write_int16
].

:- instance stream.writer(text_output_stream, int32, io)
    where
[
    pred(put/4) is write_int32
].

:- instance stream.writer(text_output_stream, int64, io)
    where
[
    pred(put/4) is write_int64
].

:- instance stream.writer(text_output_stream, uint, io)
    where
[
    pred(put/4) is write_uint
].

:- instance stream.writer(text_output_stream, uint8, io)
    where
[
    pred(put/4) is write_uint8
].

:- instance stream.writer(text_output_stream, uint16, io)
    where
[
    pred(put/4) is write_uint16
].

:- instance stream.writer(text_output_stream, uint32, io)
    where
[
    pred(put/4) is write_uint32
].

:- instance stream.writer(text_output_stream, uint64, io)
    where
[
    pred(put/4) is write_uint64
].

:- instance stream.writer(text_output_stream, string, io)
    where
[
    pred(put/4) is write_string
].

:- instance stream.writer(text_output_stream, univ, io)
    where
[
    pred(put/4) is stream.string_writer.write_univ
].

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

:- instance stream.seekable(binary_output_stream, io)
    where
[
    ( seek(Stream, Whence0, OffSet, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        seek_binary_output(Stream, Whence, OffSet, !IO)
    ),
    ( seek64(Stream, Whence0, Offset, !IO) :-
        Whence = stream_whence_to_io_whence(Whence0),
        seek_binary_output64(Stream, Whence, Offset, !IO)
    )
].

%---------------------%

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

%---------------------%
%
% Functions used only by concrete instance methods.
%

:- func result1_to_stream_result1(io.result(T)) = stream.result(T, io.error).

result1_to_stream_result1(ok(T)) = ok(T).
result1_to_stream_result1(eof) = eof.
result1_to_stream_result1(error(Error)) = error(Error).

:- func result0_to_stream_result0(io.result) = stream.result(io.error).

result0_to_stream_result0(ok) = ok.
result0_to_stream_result0(eof) = eof.
result0_to_stream_result0(error(Error)) = error(Error).

:- func stream_whence_to_io_whence(stream.whence) = io.whence.

stream_whence_to_io_whence(set) = set.
stream_whence_to_io_whence(cur) = cur.
stream_whence_to_io_whence(end) = end.

%---------------------------------------------------------------------------%
%
% Error handling.
%
% The predicates interpreting result codes ought to stay in io.m,
% because moving them to e.g. io.error_util.m would prevent them
% from being inlined into their call sites above at low optimization levels,
% and some of those call sites (e.g. in read_char) can be expected to be
% heavily used in some programs.
%

:- type result_code
    --->    result_code_ok
    ;       result_code_eof
    ;       result_code_error.

:- pragma foreign_export_enum("C", result_code/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("C#", result_code/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("Java", result_code/0,
    [prefix("ML_"), uppercase]).

:- pred interpret_result_code0(result_code::in, system_error::in,
    io.result::out, io::di, io::uo) is det.
:- pragma inline(pred(interpret_result_code0/5)).

interpret_result_code0(ResultCode, Error, Result, !IO) :-
    (
        ResultCode = result_code_ok,
        Result = ok
    ;
        ResultCode = result_code_eof,
        Result = eof
    ;
        ResultCode = result_code_error,
        make_io_error_from_system_error(Error, "read failed: ", IOError, !IO),
        Result = error(IOError)
    ).

:- pred interpret_result_code1(result_code::in, system_error::in,
    T::in, io.result(T)::out, io::di, io::uo) is det.
:- pragma inline(pred(interpret_result_code1/6)).

interpret_result_code1(ResultCode, Error, Value, Result, !IO) :-
    (
        ResultCode = result_code_ok,
        Result = ok(Value)
    ;
        ResultCode = result_code_eof,
        Result = eof
    ;
        ResultCode = result_code_error,
        make_io_error_from_system_error(Error, "read failed: ", IOError, !IO),
        Result = error(IOError)
    ).

%---------------------%

:- type maybe_incomplete_result_code
    --->    mirc_ok
    ;       mirc_eof
    ;       mirc_incomplete
    ;       mirc_error.

:- pragma foreign_export_enum("C", maybe_incomplete_result_code/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("C#", maybe_incomplete_result_code/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("Java", maybe_incomplete_result_code/0,
    [prefix("ML_"), uppercase]).

:- pred interpret_maybe_incomplete_result_code(
    maybe_incomplete_result_code::in, system_error::in, list(uint8)::in,
    T::in, maybe_incomplete_result(T)::out, io::di, io::uo) is det.
:- pragma inline(pred(interpret_maybe_incomplete_result_code/7)).

interpret_maybe_incomplete_result_code(ResultCode, Error, IncompleteBytes,
        Value, Result, !IO) :-
    (
        ResultCode = mirc_ok,
        Result = ok(Value)
    ;
        ResultCode = mirc_eof,
        Result = eof
    ;
        ResultCode = mirc_incomplete,
        Result = incomplete(IncompleteBytes)
    ;
        ResultCode = mirc_error,
        make_io_error_from_system_error(Error, "read failed: ", IOError, !IO),
        Result = error(IOError)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_UNISTD_H
    #include <unistd.h>
#endif
#ifdef MR_HAVE_SYS_STAT_H
    #include <sys/stat.h>
#endif
#include ""mercury_types.h""            // for MR_Integer
#include ""mercury_library_types.h""    // for MercuryFilePtr
#include ""mercury_int.h""              // for MR_*_reverse_bytes

#include ""mercury_init.h""
#include ""mercury_wrapper.h""
#include ""mercury_type_info.h""
#include ""mercury_errno_name.h""
#include ""mercury_file.h""
#include ""mercury_heap.h""
#include ""mercury_misc.h""
#include ""mercury_runtime_util.h""
#include ""mercury_report_stats.h""
#include ""mercury_windows_error_name.h""

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>

#ifdef MR_HAVE_SYS_WAIT_H
  #include <sys/wait.h>     // for WIFEXITED, WEXITSTATUS, etc.
#endif

#ifdef MR_WIN32
  #include ""mercury_windows.h""
#endif

extern MR_Word          ML_io_user_globals;
#ifdef MR_THREAD_SAFE
    extern MercuryLock  ML_io_user_globals_lock;
#endif

#if 0
    extern MR_Word      ML_io_ops_table;
#endif

void                    mercury_init_io(void);

#ifdef MR_WIN32
    wchar_t             *ML_utf8_to_wide(const char *s);
    char                *ML_wide_to_utf8(const wchar_t *ws,
                            MR_AllocSiteInfoPtr alloc_id);
#endif
").

:- pragma foreign_code("C", "
MR_Word         ML_io_user_globals;
#ifdef MR_THREAD_SAFE
    MercuryLock ML_io_user_globals_lock;
#endif

#if 0
    MR_Word     ML_io_ops_table;
#endif

static void
mercury_set_binary_mode(FILE *f)
{
#if defined(MR_MSVC) || defined(MR_MINGW)
    // Calling fdopen with 'b' in the mode string does not necessarily put the
    // standard input or standard output file into binary translation mode on
    // Windows. This is the case with MinGW and MSVC. The cause is likely the
    // MSVC CRT. The solution is to change the mode on the file after opening.
    _setmode(_fileno(f), _O_BINARY);
#endif
}

void
mercury_init_io(void)
{
    MR_mercuryfile_init(stdin,  1, &mercury_stdin);
    MR_mercuryfile_init(stdout, 1, &mercury_stdout);
    MR_mercuryfile_init(stderr, 1, &mercury_stderr);

    MR_mercuryfile_init(NULL, 1, &mercury_stdin_binary);
    MR_mercuryfile_init(NULL, 1, &mercury_stdout_binary);

    mercury_current_text_input_index =    MR_new_thread_local_mutable_index();
    mercury_current_text_output_index =   MR_new_thread_local_mutable_index();
    mercury_current_binary_input_index =  MR_new_thread_local_mutable_index();
    mercury_current_binary_output_index = MR_new_thread_local_mutable_index();

#if defined(MR_HAVE_FDOPEN) && (defined(MR_HAVE_FILENO) || defined(fileno)) \
        && defined(MR_HAVE_DUP)
    MR_file(mercury_stdin_binary) = fdopen(dup(fileno(stdin)), ""rb"");
    if (MR_file(mercury_stdin_binary) != NULL) {
        mercury_set_binary_mode(MR_file(mercury_stdin_binary));
    } else {
        // The call to fdopen() may fail if stdin is not available.
        // We don't abort since we still want Mercury programs to be runnable
        // in such a circumstance (aside from those that use stdin).
        // For the same reason, we treat binary stdout identically below.
        //
        // NOTE: some versions of nohup may also cause the above call to
        // fdopen() to fail, because they redirect stdin to /dev/null
        // in *write* mode. Setting binary stdin to stdin in such a case
        // also ensures that we work with those versions of nohup.
        MR_file(mercury_stdin_binary) = stdin;
    }

    MR_file(mercury_stdout_binary) = fdopen(dup(fileno(stdout)), ""wb"");
    if (MR_file(mercury_stdout_binary) != NULL) {
        mercury_set_binary_mode(MR_file(mercury_stdout_binary));
    } else {
        MR_file(mercury_stdout_binary) = stdout;
    }
#else
    // XXX Standard ANSI/ISO C provides no way to set stdin/stdout
    // to binary mode. I guess we just have to punt...
    MR_file(mercury_stdin_binary) = stdin;
    MR_file(mercury_stdout_binary) = stdout;
#endif

#ifdef MR_THREAD_SAFE
    pthread_mutex_init(&ML_io_stream_db_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&ML_io_user_globals_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&ML_io_next_stream_id_lock, MR_MUTEX_ATTR);
#endif
}

#ifdef MR_WIN32

// Accessing Unicode file names on Windows requires that we use the functions
// taking wide character strings.
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

#endif // MR_WIN32
").

%---------------------%

:- pragma foreign_code("Java", "
    public static univ.Univ_0 ML_io_user_globals = null;
").

%---------------------%

:- pragma foreign_code("C#", "
// The ML_ prefixes here are not really needed,
// since the C# code all gets generated inside a class,
// but we keep them for consistency with the C code.

public static univ.Univ_0       ML_io_user_globals;
").

%---------------------------------------------------------------------------%
:- end_module io.
%---------------------------------------------------------------------------%
