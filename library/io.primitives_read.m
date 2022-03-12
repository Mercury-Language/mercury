%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.primitives_read.m.
%
% This file handles the details of reading in values of primitive types.
%
% We communicate results from foreign_procs as separate simple arguments
% so the C/Java/etc code does not depend on how Mercury stores its
% discriminated union data types. It also avoids memory allocation in
% inner loops.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.primitives_read.
:- interface.

:- import_module char.

    % Reads a character (code point) from specified stream. This may
    % involve converting external character encodings into Mercury's internal
    % character representation and (for text streams) converting OS line
    % indicators, e.g. CR-LF for Windows, to '\n' characters.
    %
:- pred read_char_code(input_stream::in, result_code::out, char::out,
    system_error::out, io::di, io::uo) is det.

:- pred putback_char_2(stream::in, char::in, bool::out, io::di, io::uo) is det.

%---------------------%

    % Reads a byte from specified stream.
    %
:- pred read_byte_val(input_stream::in, result_code::out, int::out,
    system_error::out, io::di, io::uo) is det.

:- pred putback_byte_2(stream::in, int::in, bool::out, io::di, io::uo) is det.

:- pred putback_uint8_2(stream::in, uint8::in, bool::out,
    io::di, io::uo) is det.

%---------------------%

:- pred do_read_binary_uint16(stream::in, byte_order::in,
    maybe_incomplete_result_code::out, uint16::out, list(uint8)::out,
    system_error::out, io::di, io::uo) is det.

:- pred do_read_binary_uint32(stream::in, byte_order::in,
    maybe_incomplete_result_code::out, uint32::out, list(uint8)::out,
    system_error::out, io::di, io::uo) is det.

:- pred do_read_binary_uint64(stream::in, byte_order::in,
    maybe_incomplete_result_code::out, uint64::out, list(uint8)::out,
    system_error::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#include ""mercury_types.h""            // for MR_Integer
#include ""mercury_int.h""              // for MR_*_reverse_bytes

#include <inttypes.h>
").

%---------------------------------------------------------------------------%

read_char_code(input_stream(Stream), ResultCode, Char, Error, !IO) :-
    read_char_code_2(Stream, ResultCode, Char, Error, !IO).

:- pred read_char_code_2(stream::in, result_code::out, char::out,
    system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_char_code_2(Stream::in, ResultCode::out, Char::out, Error::out,
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
        ResultCode = ML_RESULT_CODE_OK;
        Char = uc;
        Error = 0;
    } else if (c == EOF) {
        if (MR_FERROR(*Stream)) {
            ResultCode = ML_RESULT_CODE_ERROR;
            Error = errno;
            Char = 0;
        } else {
            ResultCode = ML_RESULT_CODE_EOF;
            Error = 0;
            Char = 0;
        }
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
                    // Illegal byte sequence whether EOF or I/O error.
                    ResultCode = ML_RESULT_CODE_ERROR;
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
                    ResultCode = ML_RESULT_CODE_ERROR;
                    Error = EILSEQ;
                    Char = 0;
                } else {
                    ResultCode = ML_RESULT_CODE_OK;
                    Char = c;
                    Error = 0;
                }
            }
        } else {
            // Invalid lead byte.
            ResultCode = ML_RESULT_CODE_ERROR;
            Error = EILSEQ;
            Char = 0;
        }
    }
").

:- pragma foreign_proc("C#",
    read_char_code_2(File::in, ResultCode::out, Char::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    try {
        int c = io.mercury_getc(mf);
        if (c == -1) {
            ResultCode = io.ML_RESULT_CODE_EOF;
            Char = 0;
        } else {
            ResultCode = io.ML_RESULT_CODE_OK;
            Char = c;
        }
        Error = null;
    } catch (System.Exception e) {
        ResultCode = io.ML_RESULT_CODE_ERROR;
        Char = 0;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    read_char_code_2(File::in, ResultCode::out, CharCode::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        int c = ((io.MR_TextInputFile) File).read_char();
        if (c == -1) {
            ResultCode = io.ML_RESULT_CODE_EOF;
            CharCode = 0;
        } else {
            ResultCode = io.ML_RESULT_CODE_OK;
            CharCode = c;
        }
        Error = null;
    } catch (java.io.IOException e) {
        ResultCode = io.ML_RESULT_CODE_ERROR;
        CharCode = 0;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

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
        // This requires multiple pushback in the underlying C library.
        char        buf[5];
        ML_ssize_t  len;
        len = MR_utf8_encode(buf, Character);
        // XXX ILSEQ Error if len==0
        for (; len > 0; len--) {
            if (MR_UNGETCH(*mf, buf[len - 1]) == EOF) {
                Ok = MR_FALSE;
                break;
            }
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

:- pragma foreign_proc("Java",
    putback_char_2(File::in, Character::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ((io.MR_TextInputFile) File).ungetc(Character);
    Ok = bool.YES;
").

%---------------------------------------------------------------------------%

read_byte_val(input_stream(Stream), Result, ByteVal, Error, !IO) :-
    read_byte_val_2(Stream, Result, ByteVal, Error, !IO).

:- pred read_byte_val_2(stream::in, result_code::out, int::out,
    system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_byte_val_2(Stream::in, ResultCode::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    int b = mercury_get_byte(Stream);
    if (b == EOF) {
        if (MR_FERROR(*Stream)) {
            ResultCode = ML_RESULT_CODE_ERROR;
            Error = errno;
        } else {
            ResultCode = ML_RESULT_CODE_EOF;
            Error = 0;
        }
        ByteVal = 0;
    } else {
        ResultCode = ML_RESULT_CODE_OK;
        ByteVal = b;
        Error = 0;
    }
").

:- pragma foreign_proc("C#",
    read_byte_val_2(File::in, ResultCode::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback != -1) {
        ResultCode = io.ML_RESULT_CODE_OK;
        ByteVal = mf.putback;
        Error = null;
        mf.putback = -1;
    } else {
        try {
            int b = mf.stream.ReadByte();
            if (b == -1) {
                ResultCode = io.ML_RESULT_CODE_EOF;
                ByteVal = 0;
            } else {
                ResultCode = io.ML_RESULT_CODE_OK;
                ByteVal = b;
            }
            Error = null;
        } catch (System.Exception e) {
            ResultCode = io.ML_RESULT_CODE_ERROR;
            ByteVal = 0;
            Error = e;
        }
    }
").

:- pragma foreign_proc("Java",
    read_byte_val_2(File::in, ResultCode::out, ByteVal::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        int b = ((io.MR_BinaryInputFile) File).read_byte();
        if (b == -1) {
            ResultCode = io.ML_RESULT_CODE_EOF;
            ByteVal = 0;
        } else {
            ResultCode = io.ML_RESULT_CODE_OK;
            ByteVal = b;
        }
        Error = null;
    } catch (java.io.IOException e) {
        ResultCode = io.ML_RESULT_CODE_ERROR;
        ByteVal = 0;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

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
    putback_byte_2(File::in, Byte::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ((io.MR_BinaryInputFile) File).ungetc((byte) Byte);
    Ok = bool.YES;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    putback_uint8_2(Stream::in, UInt8::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    MercuryFilePtr mf = Stream;
    if (MR_UNGETCH(*mf, UInt8) == EOF) {
        Ok = MR_FALSE;
    } else {
        Ok = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    putback_uint8_2(File::in, UInt8::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    io.MR_MercuryFileStruct mf = File;
    if (mf.putback == -1) {
        mf.putback = UInt8;
        Ok = mr_bool.YES;
    } else {
        Ok = mr_bool.NO;
    }
").

:- pragma foreign_proc("Java",
    putback_uint8_2(File::in, UInt8::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ((io.MR_BinaryInputFile) File).ungetc((byte) UInt8);
    Ok = bool.YES;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_read_binary_uint16(Stream::in, ByteOrder::in, ResultCode::out,
        UInt16::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        tabled_for_io],
"
    ML_do_read_binary_uintN(2, 16, Stream, ByteOrder, ResultCode, UInt16,
        IncompleteBytes, Error);
").

:- pragma foreign_proc("C#",
    do_read_binary_uint16(Stream::in, ByteOrder::in, ResultCode::out,
        UInt16::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] buffer = new byte[2];
    io.MR_MercuryFileStruct mf = Stream;
    UInt16 = 0;
    IncompleteBytes = list.empty_list();

    int nread = 0;

    if (mf.putback != -1) {
        buffer[nread] = (byte) mf.putback;
        nread++;
        mf.putback = -1;
    }

    try {
        for ( ; nread < 2; nread++) {
            int b = mf.stream.ReadByte();
            if (b == -1) {
                break;
            }
            buffer[nread] = (byte) b;
        }
        if (nread < 2) {
            if (nread > 0) {
                ResultCode = io.ML_MIRC_INCOMPLETE;
                IncompleteBytes = list.cons(buffer[0], IncompleteBytes);
            } else {
                ResultCode = io.ML_MIRC_EOF;
            }
        } else {
            ResultCode = io.ML_MIRC_OK;
            if (ByteOrder == io.ML_LITTLE_ENDIAN) {
                UInt16 = (ushort) (buffer[1] << 8 | (buffer[0] & 0x00ff));
            } else {
                UInt16 = (ushort) (buffer[0] << 8 | (buffer[1] & 0x00ff));
            }
        }
        Error = null;
    } catch (System.Exception e) {
        ResultCode = io.ML_MIRC_ERROR;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_read_binary_uint16(Stream::in, ByteOrder::in, ResultCode::out,
        UInt16::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] buffer = new byte[2];
    io.MR_BinaryInputFile mf = (io.MR_BinaryInputFile) Stream;
    UInt16 = 0;
    IncompleteBytes = list.empty_list();

    try {
        int nread;
        for (nread = 0; nread < 2; nread++) {
            int next = mf.read_byte();
            if (next == -1) {
                break;
            }
            buffer[nread] = (byte) next;
        }
        if (nread < 2) {
            if (nread > 0) {
                ResultCode = io.ML_MIRC_INCOMPLETE;
                IncompleteBytes = list.cons(buffer[0], IncompleteBytes);
            } else {
                ResultCode = io.ML_MIRC_EOF;
            }
        } else {
            ResultCode = io.ML_MIRC_OK;
            if (ByteOrder == io.ML_LITTLE_ENDIAN) {
                UInt16 = (short) (buffer[1] << 8 | (buffer[0] & 0x00ff));
            } else {
                UInt16 = (short) (buffer[0] << 8 | (buffer[1] & 0x00ff));
            }
        }
        Error = null;
    } catch (java.lang.Exception e) {
        ResultCode = io.ML_MIRC_ERROR;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_read_binary_uint32(Stream::in, ByteOrder::in, ResultCode::out,
        UInt32::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        tabled_for_io],
"
    ML_do_read_binary_uintN(4, 32, Stream, ByteOrder, ResultCode, UInt32,
        IncompleteBytes, Error);
").

:- pragma foreign_proc("C#",
    do_read_binary_uint32(Stream::in, ByteOrder::in, ResultCode::out,
        UInt32::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] buffer = new byte[4];
    io.MR_MercuryFileStruct mf = Stream;
    UInt32 = 0;
    IncompleteBytes = list.empty_list();

    int nread = 0;

    if (mf.putback != -1) {
        buffer[nread] = (byte) mf.putback;
        nread++;
        mf.putback = -1;
    }

    try {
        for ( ; nread < 4; nread++) {
            int b = mf.stream.ReadByte();
            if (b == -1) {
                break;
            }
            buffer[nread] = (byte) b;
        }
        if (nread < 4) {
            if (nread > 0) {
                ResultCode = io.ML_MIRC_INCOMPLETE;
                for (int i = nread - 1; i >= 0; i--) {
                    IncompleteBytes = list.cons(buffer[i], IncompleteBytes);
                }
            } else {
                ResultCode = io.ML_MIRC_EOF;
            }
        } else {
            ResultCode = io.ML_MIRC_OK;
            if (ByteOrder == io.ML_LITTLE_ENDIAN) {
                UInt32 = (uint) (
                    buffer[3] << 24 |
                    buffer[2] << 16 |
                    buffer[1] << 8  |
                    buffer[0]);
            } else {
                UInt32 = (uint) (
                    buffer[0] << 24 |
                    buffer[1] << 16 |
                    buffer[2] << 8  |
                    buffer[3]);
            }
        }
        Error = null;
    } catch (System.Exception e) {
        ResultCode = io.ML_MIRC_ERROR;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_read_binary_uint32(Stream::in, ByteOrder::in, ResultCode::out,
        UInt32::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] buffer = new byte[4];
    io.MR_BinaryInputFile mf = (io.MR_BinaryInputFile) Stream;
    UInt32 = 0;
    IncompleteBytes = list.empty_list();

    try {
        int nread;
        for (nread = 0; nread < 4; nread++) {
            int next = mf.read_byte();
            if (next == -1) {
                break;
            }
            buffer[nread] = (byte) next;
        }
        if (nread < 4) {
            if (nread > 0) {
                ResultCode = io.ML_MIRC_INCOMPLETE;
                for (int i = nread - 1; i >= 0; i--) {
                    IncompleteBytes = list.cons(buffer[i], IncompleteBytes);
                }
            } else {
                ResultCode = io.ML_MIRC_EOF;
            }
        } else {
            ResultCode = io.ML_MIRC_OK;
            if (ByteOrder == io.ML_LITTLE_ENDIAN) {
                UInt32 =
                    (buffer[3] & 0xff) << 24 |
                    (buffer[2] & 0xff) << 16 |
                    (buffer[1] & 0xff) << 8  |
                    (buffer[0] & 0xff);
            } else {
                UInt32 =
                    (buffer[0] & 0xff) << 24 |
                    (buffer[1] & 0xff) << 16 |
                    (buffer[2] & 0xff) << 8  |
                    (buffer[3] & 0xff);
            }
        }
        Error = null;
    } catch (java.lang.Exception e) {
        ResultCode = io.ML_MIRC_ERROR;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    do_read_binary_uint64(Stream::in, ByteOrder::in, ResultCode::out,
        UInt64::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        tabled_for_io],
"
    ML_do_read_binary_uintN(8, 64, Stream, ByteOrder, ResultCode, UInt64,
        IncompleteBytes, Error);
").

:- pragma foreign_proc("C#",
    do_read_binary_uint64(Stream::in, ByteOrder::in, ResultCode::out,
        UInt64::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] buffer = new byte[8];
    io.MR_MercuryFileStruct mf = Stream;
    UInt64 = 0;
    IncompleteBytes = list.empty_list();

    int nread = 0;

    if (mf.putback != -1) {
        buffer[nread] = (byte) mf.putback;
        nread++;
        mf.putback = -1;
    }

    try {
        for ( ; nread < 8; nread++) {
            int b = mf.stream.ReadByte();
            if (b == -1) {
                break;
            }
            buffer[nread] = (byte) b;
        }
        if (nread < 8) {
            if (nread > 0) {
                ResultCode = io.ML_MIRC_INCOMPLETE;
                for (int i = nread - 1; i >=0; i--) {
                    IncompleteBytes = list.cons(buffer[i], IncompleteBytes);
                }
            } else {
                ResultCode = io.ML_MIRC_EOF;
            }
        } else {
            ResultCode = io.ML_MIRC_OK;
            if (ByteOrder == io.ML_LITTLE_ENDIAN) {
                UInt64 = (ulong) (
                    (ulong) buffer[7] << 56 |
                    (ulong) buffer[6] << 48 |
                    (ulong) buffer[5] << 40 |
                    (ulong) buffer[4] << 32 |
                    (ulong) buffer[3] << 24 |
                    (ulong) buffer[2] << 16 |
                    (ulong) buffer[1] << 8  |
                    (ulong) buffer[0]);
            } else {
                UInt64 = (ulong) (
                    (ulong) buffer[0] << 56 |
                    (ulong) buffer[1] << 48 |
                    (ulong) buffer[2] << 40 |
                    (ulong) buffer[3] << 32 |
                    (ulong) buffer[4] << 24 |
                    (ulong) buffer[5] << 16 |
                    (ulong) buffer[6] << 8  |
                    (ulong) buffer[7]);
            }
        }
        Error = null;
    } catch (System.Exception e) {
        ResultCode = io.ML_MIRC_ERROR;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_read_binary_uint64(Stream::in, ByteOrder::in, ResultCode::out,
        UInt64::out, IncompleteBytes::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] buffer = new byte[8];
    io.MR_BinaryInputFile mf = (io.MR_BinaryInputFile) Stream;
    UInt64 = 0;
    IncompleteBytes = list.empty_list();

    try {
        int nread;
        for (nread = 0; nread < 8; nread++) {
            int next = mf.read_byte();
            if (next == -1) {
                break;
            }
            buffer[nread] = (byte) next;
        }
        if (nread < 8) {
            if (nread > 0) {
                ResultCode = io.ML_MIRC_INCOMPLETE;
                for (int i = nread - 1; i >= 0; i--) {
                    IncompleteBytes = list.cons(buffer[i], IncompleteBytes);
                }
            } else {
                ResultCode = io.ML_MIRC_EOF;
            }
        } else {
            ResultCode = io.ML_MIRC_OK;
            if (ByteOrder == io.ML_LITTLE_ENDIAN) {
                UInt64 =
                    (long) (buffer[7] & 0xff) << 56 |
                    (long) (buffer[6] & 0xff) << 48 |
                    (long) (buffer[5] & 0xff) << 40 |
                    (long) (buffer[4] & 0xff) << 32 |
                    (long) (buffer[3] & 0xff) << 24 |
                    (long) (buffer[2] & 0xff) << 16 |
                    (long) (buffer[1] & 0xff) << 8  |
                    (long) (buffer[0] & 0xff);
            } else {
                UInt64 =
                    (long) (buffer[0] & 0xff) << 56 |
                    (long) (buffer[1] & 0xff) << 48 |
                    (long) (buffer[2] & 0xff) << 40 |
                    (long) (buffer[3] & 0xff) << 32 |
                    (long) (buffer[4] & 0xff) << 24 |
                    (long) (buffer[5] & 0xff) << 16 |
                    (long) (buffer[6] & 0xff) << 8  |
                    (long) (buffer[7] & 0xff);
            }
        }
        Error = null;
    } catch (java.lang.Exception e) {
        ResultCode = io.ML_MIRC_ERROR;
        Error = e;
    }
").

%---------------------%
%
% C implementation of reading multibyte integers from binary streams.
%

:- pragma foreign_decl("C", "
// ML_N_BIT_UINT_T(n) expands to the name of an n-bit unsigned integer type
// in C, if N is 8, 16, 32 or 64.
//
#define ML_N_BIT_INT_T(n) \
    MR_PASTE3(uint, n, _t)

// ML_REVERSE_BYTES_FUNC(n) expands to the name a function exported by the
// Mercury runtime that can be used to reverse the bytes in an n-bit
// unsigned integer, if N is 16, 32 or 64.
//
#define ML_REVERSE_BYTES_FUNC(n) \
    MR_PASTE3(MR_uint, n, _reverse_bytes)

// ML_build_uintN(int n, MR_Word byte_order, unsigned char *buffer,
//     uintN_t value):
//
// Build an n-bit unsigned integer using the bytes stored in the array
// 'buffer'. The order of the bytes in the buffer are given by 'byte_order'.
// The result is assigned to the lvalue 'value'
//
// We have two definitions of this macro, one for big-endian machines
// and one for little-endian machines.
//
#if defined(MR_BIG_ENDIAN)
#define ML_build_uintN(n, byte_order, buffer, value)                 \
    do {                                                             \
        if (byte_order == ML_LITTLE_ENDIAN) {                        \
            value = ML_REVERSE_BYTES_FUNC(n)(                        \
                *((ML_N_BIT_INT_T(n) *) buffer));                    \
        } else {                                                     \
            value = *((ML_N_BIT_INT_T(n) *) buffer);                 \
        }                                                            \
    } while (0)
#else
#define ML_build_uintN(n, byte_order, buffer, value)                 \
    do {                                                             \
        if (byte_order == ML_LITTLE_ENDIAN) {                        \
            value = *((ML_N_BIT_INT_T(n) *) buffer);                 \
        } else {                                                     \
            value = ML_REVERSE_BYTES_FUNC(n)(                        \
                *((ML_N_BIT_INT_T(n) *) buffer));                    \
        }                                                            \
    } while (0)
#endif

// ML_do_read_binary_uintN(int nbytes, int nbits, MR_Word stream,
//     MR_Word byte_order, MR_Word result_code, MR_Word result_value,
//     MR_Word result_incomplete, MR_Word result_error):
//
// This macro implements the do_read_binary_uint{16 32,64}/8 predicates.
// It expands to code for reading an 'nbits'-bit ('nbytes'-byte) unsigned
// integer from the binary stream 'stream', with the bytes in the stream
// being in 'byte_order' order.
//
// The result is returned as follows:
//
// 'result_code' is set the status code (maybe_incomplete_result_code/0)
// for the read.
// 'result_value' is the value of the integer read on a successful read
// and zero otherwise.
// 'result_incomplete' is the list of bytes read so far for an incomplete
// read, and the empty list otherwise.
// 'result_error' is the errno if an I/O error occurs, and zero otherwise.
//
#define ML_do_read_binary_uintN(nbytes, nbits, stream, byte_order,           \
       result_code, result_value, result_incomplete, result_error)           \
    do {                                                                     \
        unsigned char buffer[nbytes];                                        \
        size_t nread = MR_READ(*stream, buffer, nbytes);                     \
        result_incomplete = MR_list_empty();                                 \
                                                                             \
        if (nread < nbytes) {                                                \
            result_value = 0;                                                \
            if (MR_FERROR(*stream)) {                                        \
                result_code = ML_MIRC_ERROR,                                 \
                result_error = errno;                                        \
            } else if (nread > 0) {                                          \
                int i;                                                       \
                result_code = ML_MIRC_INCOMPLETE;                            \
                for (i = nread - 1; i >= 0; i--) {                           \
                    result_incomplete =                                      \
                        MR_list_cons(buffer[i],                              \
                        result_incomplete);                                  \
                }                                                            \
                result_error = 0;                                            \
            } else {                                                         \
                result_code = ML_MIRC_EOF;                                   \
                result_error = 0;                                            \
            }                                                                \
        } else {                                                             \
            result_code = ML_MIRC_OK;                                        \
            ML_build_uintN(nbits, byte_order, buffer, result_value);         \
            result_error = 0;                                                \
        }                                                                    \
    } while (0)
").

%---------------------------------------------------------------------------%
:- end_module io.primitives_read.
%---------------------------------------------------------------------------%
