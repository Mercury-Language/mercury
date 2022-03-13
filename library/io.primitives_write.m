%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.primitives_write.m.
%
% This file handles the details of writing out values of primitive types.
%
% We communicate results from foreign_procs as separate simple arguments
% so the C/Java/etc code does not depend on how Mercury stores its
% discriminated union data types. It also avoids memory allocation in
% inner loops.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.primitives_write.
:- interface.

:- import_module char.

:- pred do_write_char(stream::in, char::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_int(stream::in, int::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_uint(stream::in, uint::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_int8(stream::in, int8::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_uint8(stream::in, uint8::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_int16(stream::in, int16::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_uint16(stream::in, uint16::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_int32(stream::in, int32::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_uint32(stream::in, uint32::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_int64(stream::in, int64::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_uint64(stream::in, uint64::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_float(stream::in, float::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_string(stream::in, string::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_byte(stream::in, int::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint16(stream::in, uint16::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint16_le(stream::in, uint16::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint16_be(stream::in, uint16::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint32(stream::in, uint32::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint32_le(stream::in, uint32::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint32_be(stream::in, uint32::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint64(stream::in, uint64::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint64_le(stream::in, uint64::in, system_error::out,
    io::di, io::uo) is det.

:- pred do_write_binary_uint64_be(stream::in, uint64::in, system_error::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#include ""mercury_types.h""            // for MR_Integer
#include ""mercury_int.h""              // for MR_*_reverse_bytes

#include <inttypes.h>
").

%---------------------------------------------------------------------------%

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
        // XXX ILSEQ Error if len==0
        for (i = 0; i < len; i++) {
            if (MR_PUTCH(*Stream, buf[i]) < 0) {
                Error = errno;
                break;
            }
        }
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
                io.text_encoding);
        }
        System.IO.TextWriter w = stream.writer;
        if (Character == '\\n') {
            switch (stream.line_ending) {
            case io.ML_line_ending_kind.ML_raw_binary:
            case io.ML_line_ending_kind.ML_Unix_line_ending:
                mercury.io.mercury_write_codepoint(w, Character);
                break;
            case io.ML_line_ending_kind.ML_OS_line_ending:
                w.WriteLine("""");
                break;
            }
            stream.line_number++;
        } else {
            mercury.io.mercury_write_codepoint(w, Character);
        }
        Error = null;
    } catch (System.SystemException e) {
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_int64(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRId64, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_int64(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_int64(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(String.valueOf(Val));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_uint64(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    if (ML_fprintf(Stream, ""%"" PRIu64, Val) < 0) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_uint64(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        io.mercury_print_string(Stream, Val.ToString());
        Error = null;
    } catch (System.SystemException e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_uint64(Stream::in, Val::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        ((io.MR_TextOutputFile) Stream).write(
            java.lang.Long.toUnsignedString(Val));
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

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

% XXX MISSING C# do_write_float

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

do_write_float(Stream, Float, Error, !IO) :-
    do_write_string(Stream, string.float_to_string(Float), Error, !IO).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_byte(Stream::in, Byte::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    // Call putc with a strictly non-negative byte-sized integer.
    if (MR_PUTCH(*Stream, (int) ((unsigned char) Byte)) < 0) {
        Error = errno;
    } else {
        Error = 0;
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

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint16(Stream::in, U16::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (MR_WRITE(*Stream, (unsigned char *) (&U16), 2) != 2) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint16(Stream::in, U16::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(2);
        buffer.order(java.nio.ByteOrder.nativeOrder());
        buffer.putShort(U16);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 2);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint16(Stream::in, U16::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U16);
    try {
        Stream.stream.Write(bytes, 0, 2);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint16_le(Stream::in, U16::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    #if defined(MR_BIG_ENDIAN)
        U16 = MR_uint16_reverse_bytes(U16);
    #endif

    if (MR_WRITE(*Stream, (unsigned char *) (&U16), 2) != 2) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint16_le(Stream::in, U16::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U16);
    if (!System.BitConverter.IsLittleEndian) {
        System.Array.Reverse(bytes);
    }
    try {
        Stream.stream.Write(bytes, 0, 2);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint16_le(Stream::in, U16::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(2);
        buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN);
        buffer.putShort(U16);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 2);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint16_be(Stream::in, U16::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    #if defined(MR_LITTLE_ENDIAN)
        U16 = MR_uint16_reverse_bytes(U16);
    #endif

    if (MR_WRITE(*Stream, (unsigned char *) (&U16), 2) != 2) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint16_be(Stream::in, U16::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U16);
    if (System.BitConverter.IsLittleEndian) {
        System.Array.Reverse(bytes);
    }
    try {
        Stream.stream.Write(bytes, 0, 2);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint16_be(Stream::in, U16::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(2);
        // Order in a byte buffer is big endian by default.
        buffer.putShort(U16);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 2);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint32(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (MR_WRITE(*Stream, (unsigned char *) (&U32), 4) != 4) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint32(Stream::in, U32::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U32);
    try {
        Stream.stream.Write(bytes, 0, 4);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint32(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(4);
        buffer.order(java.nio.ByteOrder.nativeOrder());
        buffer.putInt(U32);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 4);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint32_le(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    #if defined(MR_BIG_ENDIAN)
        U32 = MR_uint32_reverse_bytes(U32);
    #endif

    if (MR_WRITE(*Stream, (unsigned char *) (&U32), 4) != 4) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint32_le(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U32);
    if (!System.BitConverter.IsLittleEndian) {
        System.Array.Reverse(bytes);
    }
    try {
        Stream.stream.Write(bytes, 0, 4);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint32_le(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(4);
        buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN);
        buffer.putInt(U32);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 4);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint32_be(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    #if defined(MR_LITTLE_ENDIAN)
        U32 = MR_uint32_reverse_bytes(U32);
    #endif

    if (MR_WRITE(*Stream, (unsigned char *) (&U32), 4) != 4) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint32_be(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U32);
    if (System.BitConverter.IsLittleEndian) {
        System.Array.Reverse(bytes);
    }
    try {
        Stream.stream.Write(bytes, 0, 4);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint32_be(Stream::in, U32::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(4);
        // Order in a byte buffer is big endian by default.
        buffer.putInt(U32);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 4);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint64(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (MR_WRITE(*Stream, (unsigned char *) (&U64), 8) != 8) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint64(Stream::in, U64::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U64);
    try {
        Stream.stream.Write(bytes, 0, 8);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint64(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(8);
        buffer.order(java.nio.ByteOrder.nativeOrder());
        buffer.putLong(U64);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 8);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint64_le(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    #if defined(MR_BIG_ENDIAN)
        U64 = MR_uint64_reverse_bytes(U64);
    #endif

    if (MR_WRITE(*Stream, (unsigned char *) (&U64), 8) != 8) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint64_le(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U64);
    if (!System.BitConverter.IsLittleEndian) {
        System.Array.Reverse(bytes);
    }
    try {
        Stream.stream.Write(bytes, 0, 8);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint64_le(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(8);
        buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN);
        buffer.putLong(U64);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 8);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_write_binary_uint64_be(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    #if defined(MR_LITTLE_ENDIAN)
        U64 = MR_uint64_reverse_bytes(U64);
    #endif

    if (MR_WRITE(*Stream, (unsigned char *) (&U64), 8) != 8) {
        Error = errno;
    } else {
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    do_write_binary_uint64_be(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    byte[] bytes = System.BitConverter.GetBytes(U64);
    if (System.BitConverter.IsLittleEndian) {
        System.Array.Reverse(bytes);
    }
    try {
        Stream.stream.Write(bytes, 0, 8);
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_write_binary_uint64_be(Stream::in, U64::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate(8);
        // Order in a byte buffer is big endian by default.
        buffer.putLong(U64);
        ((io.MR_BinaryOutputFile) Stream).write(buffer.array(), 0, 8);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module io.primitives_write.
%---------------------------------------------------------------------------%