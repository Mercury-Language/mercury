%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.stream_ops.m.
%
% This file handles the implementation of operations that operate
% on streams as a whole, such as opening and closing streams, reading
% or updating the current offset on streams, and finding some standard streams.
%
% We communicate results from foreign_procs as separate simple arguments
% so the C/Java/etc code does not depend on how Mercury stores its
% discriminated union data types. It also avoids memory allocation in
% inner loops.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.stream_ops.
:- interface.

    % do_open_text(File, Mode, StreamId, Stream, Error, !IO):
    % do_open_binary(File, Mode, StreamId, Stream, Error, !IO):
    %
    % Attempts to open a file in the specified mode.
    % The Mode must be a string suitable for passing to fopen(), and
    % - should not have a final "b" when calling do_open_text, but
    % - should have a final "b" when calling do_open_binary.
    % StreamId will be a unique integer identifying the open.
    % StreamId and Stream will be valid only if Error indicates
    % that an error has NOT occurred.
    %
:- pred do_open_text(string::in, string::in, int::out, stream::out,
    system_error::out, io::di, io::uo) is det.
:- pred do_open_binary(string::in, string::in, int::out, stream::out,
    system_error::out, io::di, io::uo) is det.

:- pred close_stream(stream::in, system_error::out, io::di, io::uo) is det.

%---------------------%

:- pred whence_to_int(io.whence::in, int::out) is det.

:- pred seek_binary_2(stream::in, int::in, int64::in, system_error::out,
    io::di, io::uo) is det.

:- pred binary_stream_offset_2(stream::in, int64::out, system_error::out,
    io::di, io::uo) is det.

%---------------------%

:- pred flush_text_output_2(stream::in, system_error::out,
    io::di, io::uo) is det.
:- pred flush_binary_output_2(stream::in, system_error::out,
    io::di, io::uo) is det.

%---------------------%

:- pred get_input_line_number_2(stream::in, int::out, io::di, io::uo) is det.
:- pred set_input_line_number_2(stream::in, int::in, io::di, io::uo) is det.
:- pred get_output_line_number_2(stream::in, int::out, io::di, io::uo) is det.
:- pred set_output_line_number_2(stream::in, int::in, io::di, io::uo) is det.

%---------------------%

:- func stdin_stream_2 = stream.
:- pred stdin_stream_2(stream::out, io::di, io::uo) is det.
:- pred stdin_binary_stream_2(stream::out, io::di, io::uo) is det.

:- func stdout_stream_2 = stream.
:- pred stdout_stream_2(stream::out, io::di, io::uo) is det.
:- pred stdout_binary_stream_2(stream::out, io::di, io::uo) is det.

:- func stderr_stream_2 = stream.
:- pred stderr_stream_2(stream::out, io::di, io::uo) is det.

:- pred input_stream_2(stream::out, io::di, io::uo) is det.
:- pred binary_input_stream_2(stream::out, io::di, io::uo) is det.

:- pred output_stream_2(stream::out, io::di, io::uo) is det.
:- pred binary_output_stream_2(stream::out, io::di, io::uo) is det.

%---------------------%

:- pred set_input_stream_2(stream::in, stream::out,
    io::di, io::uo) is det.
:- pred set_binary_input_stream_2(stream::in, stream::out,
    io::di, io::uo) is det.
:- pred set_output_stream_2(stream::in, stream::out,
    io::di, io::uo) is det.
:- pred set_binary_output_stream_2(stream::in, stream::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

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

:- pragma foreign_proc("C#",
    do_open_text(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Stream = mercury.io__stream_ops.mercury_open(FileName, Mode,
            mercury.io__stream_ops.ML_default_line_ending);
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
                Stream = new jmercury.io__stream_ops.MR_TextInputFile(
                    new java.io.FileInputStream(FileName));
                break;
            case 'w':
                Stream = new jmercury.io__stream_ops.MR_TextOutputFile(
                    new java.io.FileOutputStream(FileName));
                break;
            case 'a':
                Stream = new jmercury.io__stream_ops.MR_TextOutputFile(
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

%---------------------%

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
    do_open_binary(FileName::in, Mode::in, StreamId::out, Stream::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Stream = mercury.io__stream_ops.mercury_open(FileName, Mode,
            mercury.io__stream_ops.ML_line_ending_kind.ML_raw_binary);
        StreamId = Stream.id;
        Error = null;
    } catch (System.Exception e) {
        StreamId = -1;
        Stream = null;
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
                Stream = new jmercury.io__stream_ops.MR_BinaryInputFile(
                    new java.io.FileInputStream(FileName));
                break;
            case 'w':
                Stream = new jmercury.io__stream_ops.MR_BinaryOutputFile(
                    new java.io.FileOutputStream(FileName));
                break;
            case 'a':
                Stream = new jmercury.io__stream_ops.MR_BinaryOutputFile(
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

%---------------------%

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
        mercury.io__stream_ops.mercury_close(Stream);
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

%---------------------------------------------------------------------------%

whence_to_int(set, 0).
whence_to_int(cur, 1).
whence_to_int(end, 2).

%---------------------%

:- pragma foreign_proc("C",
    seek_binary_2(Stream::in, Flag::in, Off::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    static const int seek_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END };

    // XXX check if the stream is seekable.
    if (MR_IS_FILE_STREAM(*Stream)) {
        if (MR_fseek(MR_file(*Stream), Off, seek_flags[Flag]) < 0) {
            Error = errno;
        } else {
            Error = 0;
        }
    } else {
        Error = EINVAL;
    }
").
% MISSING C# seek_binary_2
:- pragma foreign_proc("Java",
    seek_binary_2(Stream::in, Flag::in, Off::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((jmercury.io__stream_ops.MR_BinaryFile) Stream).seek_binary(
            Flag, Off);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------%

:- pragma foreign_proc("C",
    binary_stream_offset_2(Stream::in, Offset::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    // XXX should check if the stream is tellable
    if (MR_IS_FILE_STREAM(*Stream)) {
        Offset = MR_ftell(MR_file(*Stream));
        if (Offset < 0) {
            Error = errno;
        } else {
            Error = 0;
        }
    } else {
        Error = EINVAL;
    }
").
% MISSING C# binary_stream_offset_2
:- pragma foreign_proc("Java",
    binary_stream_offset_2(Stream::in, Offset::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Offset = ((jmercury.io__stream_ops.MR_BinaryFile) Stream).getOffset();
        Error = null;
    } catch (java.io.IOException e) {
        Offset = -1;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
%
% The implementations of flush_text_output_2 and flush_binary_output_2
% are identical for C and C#, but they differ in Java: one casts the stream
% to MR_TextOutputFile, the other to MR_BinaryOutputFile.
%

:- pragma foreign_proc("C",
    flush_text_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (MR_FLUSH(*Stream) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").
:- pragma foreign_proc("C#",
    flush_text_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
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
    flush_text_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ((jmercury.io__stream_ops.MR_TextOutputFile) Stream).flush();
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

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
    flush_binary_output_2(Stream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        ((jmercury.io__stream_ops.MR_BinaryOutputFile) Stream).flush();
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%
%
% The implementations of get_input_line_number_2 and get_output_line_number_2
% are identical for C and C#, but they differ in Java: one casts the stream
% to MR_TextInputFile, the other to MR_TextOutputFile. Likewise for the
% predicates that set the line number.
%

:- pragma foreign_proc("C",
    get_input_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*Stream);
").
:- pragma foreign_proc("C#",
    get_input_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = Stream.line_number;
").
:- pragma foreign_proc("Java",
    get_input_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = ((jmercury.io__stream_ops.MR_TextInputFile) Stream).line_number;
").

:- pragma foreign_proc("C",
    set_input_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*Stream) = LineNum;
").
:- pragma foreign_proc("C#",
    set_input_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").
:- pragma foreign_proc("Java",
    set_input_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ((jmercury.io__stream_ops.MR_TextInputFile) Stream).line_number = LineNum;
").

%---------------------%

:- pragma foreign_proc("C",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    LineNum = MR_line_number(*Stream);
").
:- pragma foreign_proc("C#",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    LineNum = Stream.line_number;
}").
:- pragma foreign_proc("Java",
    get_output_line_number_2(Stream::in, LineNum::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    LineNum = ((jmercury.io__stream_ops.MR_TextOutputFile) Stream).line_number;
").

:- pragma foreign_proc("C",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MR_line_number(*Stream) = LineNum;
").
:- pragma foreign_proc("C#",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Stream.line_number = LineNum;
}").
:- pragma foreign_proc("Java",
    set_output_line_number_2(Stream::in, LineNum::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ((jmercury.io__stream_ops.MR_TextOutputFile) Stream).line_number = LineNum;
").

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", stdin_stream_2(out, di, uo),
    "ML_io_stdin_stream").
:- pragma foreign_export("C", stdout_stream_2(out, di, uo),
    "ML_io_stdout_stream").
:- pragma foreign_export("C", stderr_stream_2(out, di, uo),
    "ML_io_stderr_stream").

%---------------------%

:- pragma foreign_proc("C",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = &mercury_stdin;
").
:- pragma foreign_proc("C#",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = mercury.io__stream_ops.mercury_stdin;
").
:- pragma foreign_proc("Java",
    stdin_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = jmercury.io__stream_ops.mercury_stdin;
").

%---------------------%

stdin_stream_2(Stream, !IO) :-
    Stream = stdin_stream_2.

%---------------------%

:- pragma foreign_proc("C",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = &mercury_stdin_binary;
").
:- pragma foreign_proc("C#",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury.io__stream_ops.mercury_stdin_binary;
").
:- pragma foreign_proc("Java",
    stdin_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    jmercury.io__stream_ops.ensure_init_mercury_stdin_binary();
    Stream = jmercury.io__stream_ops.mercury_stdin_binary;
").

%---------------------%

:- pragma foreign_proc("C",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = &mercury_stdout;
").
:- pragma foreign_proc("C#",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = mercury.io__stream_ops.mercury_stdout;
").
:- pragma foreign_proc("Java",
    stdout_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = jmercury.io__stream_ops.mercury_stdout;
").

%---------------------%

stdout_stream_2(Stream, !IO) :-
    Stream = stdout_stream_2.

%---------------------%

:- pragma foreign_proc("C",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = &mercury_stdout_binary;
").
:- pragma foreign_proc("C#",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Stream = mercury.io__stream_ops.mercury_stdout_binary;
").
:- pragma foreign_proc("Java",
    stdout_binary_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    jmercury.io__stream_ops.ensure_init_mercury_stdout_binary();
    Stream = jmercury.io__stream_ops.mercury_stdout_binary;
").

%---------------------%

:- pragma foreign_proc("C",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = &mercury_stderr;
").
:- pragma foreign_proc("C#",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = mercury.io__stream_ops.mercury_stderr;
").
:- pragma foreign_proc("Java",
    stderr_stream_2 = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
    Stream = jmercury.io__stream_ops.mercury_stderr;
").

%---------------------%

stderr_stream_2(Stream, !IO) :-
    Stream = stderr_stream_2.

%---------------------%

:- pragma foreign_proc("C",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = mercury_current_text_input();
").
:- pragma foreign_proc("C#",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury.io__stream_ops.mercury_current_text_input;
").
:- pragma foreign_proc("Java",
    input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Stream = jmercury.io__stream_ops.mercury_current_text_input.get();
").

%---------------------%

:- pragma foreign_proc("C",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = mercury_current_binary_input();
").
:- pragma foreign_proc("C#",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury.io__stream_ops.mercury_current_binary_input;
").
:- pragma foreign_proc("Java",
    binary_input_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Stream = jmercury.io__stream_ops.mercury_current_binary_input.get();
").

%---------------------%

:- pragma foreign_proc("C",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = mercury_current_text_output();
").
:- pragma foreign_proc("C#",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury.io__stream_ops.mercury_current_text_output;
").
:- pragma foreign_proc("Java",
    output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = jmercury.io__stream_ops.mercury_current_text_output.get();
").

%---------------------%

:- pragma foreign_proc("C",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    Stream = mercury_current_binary_output();
").
:- pragma foreign_proc("C#",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Stream = mercury.io__stream_ops.mercury_current_binary_output;
").
:- pragma foreign_proc("Java",
    binary_output_stream_2(Stream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        may_not_duplicate],
"
    Stream = jmercury.io__stream_ops.mercury_current_binary_output.get();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    OutStream = mercury_current_text_input();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_text_input_index);
").
:- pragma foreign_proc("C#",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury.io__stream_ops.mercury_current_text_input;
    mercury.io__stream_ops.mercury_current_text_input = NewStream;
").
:- pragma foreign_proc("Java",
    set_input_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = jmercury.io__stream_ops.mercury_current_text_input.get();
    jmercury.io__stream_ops.mercury_current_text_input.set(
        (jmercury.io__stream_ops.MR_TextInputFile) NewStream);
").

%---------------------%

:- pragma foreign_proc("C",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    OutStream = mercury_current_binary_input();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_binary_input_index);
").
:- pragma foreign_proc("C#",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury.io__stream_ops.mercury_current_binary_input;
    mercury.io__stream_ops.mercury_current_binary_input = NewStream;
").
:- pragma foreign_proc("Java",
    set_binary_input_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = jmercury.io__stream_ops.mercury_current_binary_input.get();
    jmercury.io__stream_ops.mercury_current_binary_input.set(
        (jmercury.io__stream_ops.MR_BinaryInputFile) NewStream);
").

%---------------------%

:- pragma foreign_proc("C",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    OutStream = mercury_current_text_output();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_text_output_index);
").
:- pragma foreign_proc("C#",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury.io__stream_ops.mercury_current_text_output;
    mercury.io__stream_ops.mercury_current_text_output = NewStream;
").
:- pragma foreign_proc("Java",
    set_output_stream_2(NewStream::in, OutStream::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = jmercury.io__stream_ops.mercury_current_text_output.get();
    jmercury.io__stream_ops.mercury_current_text_output.set(
        (jmercury.io__stream_ops.MR_TextOutputFile) NewStream);
").

%---------------------%

:- pragma foreign_proc("C",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
    % no_sharing: as a value of a foreign type, an io.stream cannot be reused.
"
    OutStream = mercury_current_binary_output();
    MR_set_thread_local_mutable(MercuryFilePtr, NewStream,
        mercury_current_binary_output_index);
").
:- pragma foreign_proc("C#",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = mercury.io__stream_ops.mercury_current_binary_output;
    mercury.io__stream_ops.mercury_current_binary_output = NewStream;
").
:- pragma foreign_proc("Java",
    set_binary_output_stream_2(NewStream::in, OutStream::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    OutStream = jmercury.io__stream_ops.mercury_current_binary_output.get();
    jmercury.io__stream_ops.mercury_current_binary_output.set(
        (jmercury.io__stream_ops.MR_BinaryOutputFile) NewStream);
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#if defined(MR_WIN32)
  #include ""mercury_string.h"" // For MR_utf8_to_wide.
#endif

extern MercuryFile      mercury_stdin;
extern MercuryFile      mercury_stdout;
extern MercuryFile      mercury_stderr;
extern MercuryFile      mercury_stdin_binary;
extern MercuryFile      mercury_stdout_binary;

extern MR_Unsigned      mercury_current_text_input_index;
extern MR_Unsigned      mercury_current_text_output_index;
extern MR_Unsigned      mercury_current_binary_input_index;
extern MR_Unsigned      mercury_current_binary_output_index;

// A counter used to generate unique stream ids.
extern int              ML_next_stream_id;

#ifdef MR_THREAD_SAFE
    extern MercuryLock  ML_io_next_stream_id_lock;
#endif

MercuryFilePtr          mercury_current_text_input(void);
MercuryFilePtr          mercury_current_text_output(void);
MercuryFilePtr          mercury_current_binary_input(void);
MercuryFilePtr          mercury_current_binary_output(void);
int                     mercury_next_stream_id(void);
MercuryFilePtr          mercury_open(const char *filename,
                             const char *openmode,
                             MR_AllocSiteInfoPtr alloc_id);
int                     mercury_close(MercuryFilePtr mf);
").

:- pragma foreign_code("C", "
MercuryFile     mercury_stdin;
MercuryFile     mercury_stdout;
MercuryFile     mercury_stderr;
MercuryFile     mercury_stdin_binary;
MercuryFile     mercury_stdout_binary;

MR_Unsigned     mercury_current_text_input_index;
MR_Unsigned     mercury_current_text_output_index;
MR_Unsigned     mercury_current_binary_input_index;
MR_Unsigned     mercury_current_binary_output_index;

int             ML_next_stream_id;

#ifdef MR_THREAD_SAFE
    MercuryLock ML_io_next_stream_id_lock;
#endif
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
    // XXX We don't know whether the new stream is text or binary.
    MR_LOCK(&ML_io_next_stream_id_lock, ""io.do_open_text"");
    id = ML_next_stream_id++;
    MR_UNLOCK(&ML_io_next_stream_id_lock, ""io.do_open_text"");
    return id;
}

MercuryFilePtr
mercury_open(const char *filename, const char *openmode,
    MR_AllocSiteInfoPtr alloc_id)
{
    MercuryFilePtr  mf;
    FILE            *f;

#ifdef MR_WIN32
    f = _wfopen(MR_utf8_to_wide(filename), MR_utf8_to_wide(openmode));
#else
    f = fopen(filename, openmode);
#endif

    if (f == NULL) {
        return NULL;
    }

    // Check if the opened file is actually a directory.
    // If fileno or fstat are not available then we assume the OS would not
    // let us open a directory as a file anyway.
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

    mf = MR_GC_NEW_ATTRIB(MercuryFile, alloc_id);
    MR_mercuryfile_init(f, 1, mf);
    return mf;
}

#ifdef EBADF
  #define MR_CLOSED_FILE_ERROR  EBADF
#else
  // ANSI/ISO C guarantees that EDOM will exist.
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
    return -1;  // XXX should this be 0?
}

static int
ME_closed_stream_write(MR_StreamInfo *info, const void *buffer, size_t size)
{
    errno = MR_CLOSED_FILE_ERROR;
    return -1;  // XXX should this be 0?
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

#endif // MR_NEW_MERCURYFILE_STRUCT

int
mercury_close(MercuryFilePtr mf)
{
    // On some systems, attempting to close a file stream that has been
    // previously closed will lead to a segmentation fault. We check that
    // we have not previously closed the file stream here, so we can give
    // the user some idea about what has happened.
    if (MR_file(*mf) == NULL) {
        errno = MR_CLOSED_FILE_ERROR;
        return EOF;
    }

    if (MR_CLOSE(*mf) < 0) {
        return EOF;
    }

#ifdef MR_NEW_MERCURYFILE_STRUCT

    // MR_closed_stream is a dummy stream object containing pointers to
    // functions that always return an error indication. Doing this ensures
    // that future accesses to the file will fail nicely.

    // gcc 2.95.2 barfs on `*mf = MR_closed_stream;'
    // so we use MR_memcpy() instead.
    MR_memcpy(mf, &MR_closed_stream, sizeof(*mf));

    // XXX It would be nice to have an autoconf check for the GNU libc function
    // fopencookie(); we could use that to do a similar thing to what we do
    // in the MR_NEW_MERCURYFILE_STRUCT case.

/****
#elif defined(HAVE_FOPENCOOKIE)
    MR_file(*mf) = MR_closed_file;
****/

#else

    // We want future accesses to the file to fail nicely. Ideally they would
    // throw an exception, but that would require a check at every I/O
    // operation, and for simple operations like putchar() or getchar(),
    // that would be too expensive. Instead we just set the file pointer
    // to NULL; on systems which trap null pointer dereferences, or if
    // library/io.m is compiled with MR_assert assertions enabled
    // (i.e. -DMR_LOWLEVEL_DEBUG), this will ensure that accessing closed files
    // traps immediately rather than causing problems at some later point.
    MR_mercuryfile_init(NULL, 0, mf);

#endif // ! MR_NEW_MERCURYFILE_STRUCT

#ifndef MR_CONSERVATIVE_GC
    if (mf == &mercury_stdin ||
        mf == &mercury_stdout ||
        mf == &mercury_stderr ||
        mf == &mercury_stdin_binary ||
        mf == &mercury_stdout_binary)
    {
        // The memory for these streams is allocated statically,
        // so there is nothing to free.
    } else {
        // For the accurate GC or no GC cases, we need to explicitly deallocate
        // the memory here, to avoid a memory leak. Note that the accurate
        // collector will not reclaim io_streams, since the io.stream type
        // is defined as a foreign_type.
        MR_GC_free(mf);
    }
#endif // !MR_CONSERVATIVE_GC

    return 0;
}
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Java", "
// While strings are stored internally as UTF-16, all text file I/O
// (including stdin/stdout/stderr) is run through stream readers/writers
// that use UTF-8 encoding.
// Binary files are opened via RandomAccessFile, which supports seek,
// but not character encoding/decoding or buffering.
// Binary stdin and stdout are a special case. They are opened via
// FileInput/OutputStreams and seeking is controlled through use of
// FileChannels.
//
// The use of the methods in this implementation is not very flexible.
// You may not perform text mode operations on a binary file or vice versa.
// XXX This causes problems for io.read_binary and io.write_binary,
// for which the current implementations attempt to access binary
// streams using text mode operations (and so will definitely break.)

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
        input = new java.io.InputStreamReader(stream,
            java.nio.charset.StandardCharsets.UTF_8);
    }

    // refill_buffer():
    // Returns true if the end of the stream has not been reached.
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

    // read_char(): [Java]
    //
    // Reads one character in from a text input file using the default
    // charset decoding. Returns -1 at end of file.
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

    // read_line(): [Java]
    //
    // Reads in a line of a text input file using the default
    // charset decoding. Returns null at end of file.
    public String read_line()
        throws java.io.IOException
    {
        if (!refill_buffer()) {
            return null;
        }

        // Often, the buffer already contains a complete line.
        for (int i = buf_pos; i < buf_end; i++) {
            if (buf[i] == '\\n') {
                String s = new String(buf, buf_pos, i - buf_pos + 1);
                buf_pos = i + 1;
                line_number++;
                return s;
            }
        }

        // The buffer doesn't contain a complete line.
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

    // read_file(): [Java]
    //
    // Reads in the rest of a text input file using the default
    // charset decoding.
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
        // If necessary, shift the unread characters in the input buffer
        // to make room at the front of the buffer. If the buffer is full,
        // allocate a bigger buffer.
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
        output = new java.io.BufferedWriter(
            new java.io.OutputStreamWriter(stream,
                java.nio.charset.StandardCharsets.UTF_8));
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

        // Flush if we saw a newline.
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

    // size(): [Java]
    //
    // Returns the length of a file.
    public long size()
        throws java.io.IOException
    {
        return channel.size();
    }

    // getOffset():
    //
    // Returns the current position in a binary file.
    abstract public long getOffset() throws java.io.IOException;

    // seek(): [Java]
    //
    // Seek relative to start, current position or end depending on the
    // flag.
    public void seek_binary(int flag, long offset)
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

    // read_byte(): [Java]
    //
    // Reads one byte in from a binary file. Returns -1 at end of file.
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
    public long getOffset()
        throws java.io.IOException
    {
        return channel.position() - pushback.size();
    }

    @Override
    public void seek_binary(int flag, long offset)
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
    public long getOffset()
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

public static MR_TextInputFile  mercury_stdin =
    new MR_TextInputFile(java.lang.System.in);
public static MR_TextOutputFile mercury_stdout =
    new MR_TextOutputFile(java.lang.System.out);
public static MR_TextOutputFile mercury_stderr =
    new MR_TextOutputFile(java.lang.System.err);

// We initialize mercury_stdin_binary and mercury_stdout_binary
// only when they are needed, because the initialization code
// does not work on Google's App Engine.
public static MR_BinaryInputFile  mercury_stdin_binary = null;
public static MR_BinaryOutputFile mercury_stdout_binary = null;

public static void ensure_init_mercury_stdin_binary() {
    if (mercury_stdin_binary == null) {
        mercury_stdin_binary = new MR_BinaryInputFile(
            new java.io.FileInputStream(java.io.FileDescriptor.in));
    }
}

public static void ensure_init_mercury_stdout_binary() {
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

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
// The ML_ prefixes here are not really needed,
// since the C# code all gets generated inside a class,
// but we keep them for consistency with the C code.

// A counter used to generate unique stream ids.
static int                      ML_next_stream_id;

public enum ML_line_ending_kind {
    // File uses the usual line-ending convention
    // for the OS (e.g. CR-LF for DOS/Windows).
    ML_OS_line_ending,

    // File uses the Unix line-encoding convention.
    ML_Unix_line_ending,

    // File stores bytes.
    ML_raw_binary
};

// This specifies the default encoding used for text files.
// It must be either ML_OS_text_encoding or ML_Unix_text_encoding.
//
// XXX The initial setting for this should be controlled
// by an environment variable. (This might require moving
// the code which initializes mercury_stdin, etc.)
//
public static readonly ML_line_ending_kind ML_default_line_ending =
    ML_line_ending_kind.ML_OS_line_ending;

// Assume UTF-8 encoding on files. When writing a file, don't emit
// a byte order mark.
public static readonly System.Text.Encoding text_encoding =
    new System.Text.UTF8Encoding(false);

public class MR_MercuryFileStruct {
    public System.IO.Stream     stream;     // The stream itself.
    // The reader and/or writer for the stream.
    // At the moment, we allow a stream only to be read OR written, not both,
    // so exactly one of the following two fields will used (and be non-null)
    // for any particular stream.
    // Note also that we use the TextReader class interface to read both
    // text AND binary files, and like use the TextWriter class interface
    // to write both text and binary files, which to me (zs) looks strange,
    // given that C# also has a System.IO.BinaryReader class.
    //
    // XXX Using four subclasses, for text input, binary input,
    // text output and binary output, as the Java code above does,
    // would allow each Mercury file struct to include only the fields
    // relevant to it. I (zs) don't know whether the downcasts necessary
    // to use that design would have an acceptable cost or not.
    // XXX Note that we *could* avoid those downcasts by making the types
    // {,binary_}{input,out}_stream each be a wrapper around a *different*
    // type, instead of all four being a wrapper around the same stream type.
    // At that point, the notag wrapping would have no use left, except to
    // make the output of e.g. ""io.write(io.stdout_stream, X, !IO)"" more
    // readable when X, the value being written, is itself a stream.
    public System.IO.TextReader reader;
    public System.IO.TextWriter writer;

    // The next character or byte to read,
    // or -1 if no putback char/byte is stored.
    // This field is used only for input streams, both text and binary;
    // it is not used for output streams.
    public int                  putback;

    // DOS, Unix, or raw binary.
    // This field is used only for text streams, both input and output;
    // it is not used for binary streams.
    public ML_line_ending_kind  line_ending;

    // This field is used only for text streams, both input and output;
    // it is not used for binary streams.
    public int                  line_number;

    public int                  id;
};

static MR_MercuryFileStruct
mercury_file_init(System.IO.Stream stream,
    System.IO.TextReader reader, System.IO.TextWriter writer,
    ML_line_ending_kind line_ending)
{
    MR_MercuryFileStruct mf = new MR_MercuryFileStruct();
    mf.stream = stream;
    mf.reader = reader;
    mf.writer = writer;
    mf.putback = -1;
    mf.line_ending = line_ending;
    mf.line_number = 1;
    mf.id = ML_next_stream_id++;
    return mf;
}

public static MR_MercuryFileStruct
mercury_open(string filename, string openmode, ML_line_ending_kind line_ending)
{
    System.IO.FileMode      mode;
    System.IO.FileAccess    access;
    System.IO.FileShare     share;
    System.IO.Stream        stream = null;
    System.IO.TextReader    reader;
    System.IO.TextWriter    writer;

    // For Unix compatibility, we allow files to be read or written
    // by multiple processes simultaneously. XXX Is this a good idea?
    share = System.IO.FileShare.ReadWrite;

    if (openmode == ""r"" || openmode == ""rb"") {
        // Like '<' in Bourne shell.
        // Read a file. The file must exist already.
        mode   = System.IO.FileMode.Open;
        access = System.IO.FileAccess.Read;
        stream = System.IO.File.Open(filename, mode, access, share);
        reader = new System.IO.StreamReader(stream,
            mercury.io__stream_ops.text_encoding);
        writer = null;
    } else if (openmode == ""w"" || openmode == ""wb"") {
        // Like '>' in Bourne shell.
        // Overwrite an existing file, or create a new file.
        mode   = System.IO.FileMode.Create;
        access = System.IO.FileAccess.Write;
        stream = System.IO.File.Open(filename, mode, access, share);
        reader = null;
        writer = new System.IO.StreamWriter(stream,
            mercury.io__stream_ops.text_encoding);
    } else if (openmode == ""a"" || openmode == ""ab"") {
        // Like '>>' in Bourne shell.
        // Append to an existing file, or create a new file.
        mode   = System.IO.FileMode.Append;
        access = System.IO.FileAccess.Write;
        stream = System.IO.File.Open(filename, mode, access, share);
        reader = null;
        writer = new System.IO.StreamWriter(stream,
            mercury.io__stream_ops.text_encoding);
    } else {
        runtime.Errors.SORRY(System.String.Concat(
            ""foreign code for this function, open mode:"",
            openmode));
        // Needed to convince the C# compiler that mode and
        // access are always initialized.
        throw new System.Exception();
    }

    return mercury_file_init(new System.IO.BufferedStream(stream),
        reader, writer, line_ending);
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

%---------------------------------------------------------------------------%
:- end_module io.stream_ops.
%---------------------------------------------------------------------------%
