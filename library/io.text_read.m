%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.text_read.m.
%
% This module implements the predicates in io.m that read
% words, lines and files.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.text_read.
:- interface.

:- import_module bitmap.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred read_word_2(io.text_input_stream::in, io.result(list(char))::out,
    io::di, io::uo) is det.

%---------------------%

:- pred read_line_2(io.text_input_stream::in, result_code::out,
    system_error::out, list(char)::out, io::di, io::uo) is det.

%---------------------%

:- type read_line_as_string_result
    --->    rlas_ok
    ;       rlas_eof
    ;       rlas_null_char
    ;       rlas_error.

:- pred read_line_as_string_2(io.stream::in, bool::in,
    read_line_as_string_result::out, system_error::out, string::out,
    io::di, io::uo) is det.

%---------------------%

:- pred read_file_as_string_2(stream::in, string::out, int::out,
    system_error::out, bool::out, io::di, io::uo) is det.

%---------------------%

:- pred read_binary_file_as_bitmap_2(io.binary_input_stream::in,
    io.res(bitmap)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module char.
:- import_module int.
:- import_module int64.
:- import_module io.primitives_read.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

read_line_2(Stream, Result, Error, Chars, !IO) :-
    read_char_code(Stream, Result0, Error0, Char, !IO),
    (
        Result0 = result_code_ok,
        ( if Char = '\n' then
            Result = result_code_ok,
            Chars = [Char],
            Error = Error0
        else
            read_line_2(Stream, Result, Error, CharsTail, !IO),
            Chars = [Char | CharsTail] % lcmc
        )
    ;
        ( Result0 = result_code_eof
        ; Result0 = result_code_error
        ),
        Result = Result0,
        Chars = [],
        Error = Error0
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_export_enum("C", read_line_as_string_result/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("Java", read_line_as_string_result/0,
    [prefix("ML_"), uppercase]).

:- pragma foreign_proc("C",
    read_line_as_string_2(Stream::in, _FirstCall::in, Res::out, Error::out,
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

    Res = ML_RLAS_OK;
    Error = 0;
    for (i = 0; char_code != '\\n'; ) {
        char_code = mercury_get_byte(Stream);
        if (char_code == EOF) {
            if (i == 0) {
                if (MR_FERROR(*Stream)) {
                    Res = ML_RLAS_ERROR;
                    Error = errno;
                } else {
                    Res = ML_RLAS_EOF;
                }
            }
            break;
        }
        if (char_code == 0) {
            Res = ML_RLAS_NULL_CHAR;
            break;
        }
        read_buffer[i++] = (char) char_code;
        MR_assert(i <= read_buf_size);
        if (i == read_buf_size) {
            // Grow the read buffer.
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
    if (Res == ML_RLAS_OK) {
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
    read_line_as_string_2(Stream::in, _FirstCall::in, Res::out, Error::out,
        RetString::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, may_not_duplicate],
"
    try {
        RetString =
            ((jmercury.io__stream_ops.MR_TextInputFile) Stream).read_line();
        if (RetString != null) {
            Res = ML_RLAS_OK;
        } else {
            Res = ML_RLAS_EOF;
        }
        Error = null;
    } catch (java.io.IOException e) {
        Res = ML_RLAS_ERROR;
        RetString = """";
        Error = e;
    }
").

read_line_as_string_2(Stream, FirstCall, Res, Error, String, !IO) :-
    % XXX This is terribly inefficient, a better approach would be
    % to use a buffer like what is done for io.read_file_as_string.
    read_char_code(text_input_stream(Stream), ResultCode, Error0, Char, !IO),
    (
        ResultCode = result_code_ok,
        ( if Char = '\n' then
            Res = rlas_ok,
            String = "\n",
            Error = Error0
        else if char.to_int(Char, 0) then
            Res = rlas_null_char,
            String = "",
            Error = Error0
        else
            read_line_as_string_2(Stream, no, Res, Error, String0, !IO),
            string.first_char(String, Char, String0)
        )
    ;
        ResultCode = result_code_eof,
        (
            FirstCall = yes,
            Res = rlas_eof
        ;
            FirstCall = no,
            Res = rlas_ok
        ),
        String = "",
        Error = Error0
    ;
        ResultCode = result_code_error,
        Res = rlas_error,
        String = "",
        Error = Error0
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    read_file_as_string_2(Stream::in, String::out, NumCUs::out,
        Error::out, NullCharError::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    StringBuilder sb = new StringBuilder();
    try {
        ((jmercury.io__stream_ops.MR_TextInputFile) Stream).read_file(sb);
        Error = null;
    } catch (java.io.IOException e) {
        Error = e;
    }
    String = sb.toString();
    NumCUs = String.length();
    NullCharError = bool.NO;
").

read_file_as_string_2(Stream, Str, NumCUs, Error, NullCharError, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    input_stream_file_size(text_input_stream(Stream), FileSize, !IO),
    ( if FileSize >= 0 then
        % When targeting C, this reserves just enough space for all the bytes
        % in the file, plus the final NUL character.
        %
        % When targeting C#, this reserves one slot in an array of code points
        % for each byte in the file, plus the NUL. This means that the buffer
        % we reserve may be bigger than needed. How much bigger depends on
        % the number of code points in the file that take more than one
        % UTF-16 code units.
        BufferSize0 = FileSize + 1
    else
        BufferSize0 = 4000
    ),
    alloc_buffer(BufferSize0, Buffer0),
    % Read the file into the buffer (resizing it as we go if necessary),
    % convert the buffer into a string, and see if anything went wrong.
    %
    % When targeting C, Pos counts UTF-8 code *units* (in the usual case
    % where the input is valid UTF-8; otherwise, it counts bytes).
    % When targeting C#, Pos counts code *points*.
    % When targeting Java, the foreign_proc above replaces this clause.
    Pos0 = 0,
    read_file_as_string_loop(text_input_stream(Stream), Buffer0, BufferSize0,
        Pos0, Str, NumCUs, Error, NullCharError, !IO).

:- pred read_file_as_string_loop(text_input_stream::in, buffer::buffer_di,
    int::in, int::in, string::out, int::out, system_error::out, bool::out,
    io::di, io::uo) is det.
% This predicate is not used when compiling to Java; this pragma avoids
% a warning even in that case.
:- pragma consider_used(pred(read_file_as_string_loop/10)).

read_file_as_string_loop(Stream, !.Buffer, BufferSize0, !.Pos,
        Str, NumCUs, Error, NullCharError, !IO) :-
    Stream = text_input_stream(RealStream),
    read_into_buffer(RealStream, !Buffer, BufferSize0, !Pos, Error0, !IO),
    ( if !.Pos < BufferSize0 then
        % Buffer is not full: end-of-file or error.
        ( if
            buffer_and_pos_to_string_and_length(!.Buffer, !.Pos,
                StrPrime, NumCUsPrime)
        then
            Str = StrPrime,
            NumCUs = NumCUsPrime,
            NullCharError = no
        else
            Str = "",
            NumCUs = 0,
            NullCharError = yes
        ),
        Error = Error0
    else if !.Pos = BufferSize0 then
        % Buffer is full; make room for more of the file.
        % Doubling its size should catch up to its actual size quickly.
        BufferSize1 = BufferSize0 * 2,
        resize_buffer(BufferSize0, BufferSize1, !Buffer),
        read_file_as_string_loop(Stream, !.Buffer, BufferSize1, !.Pos,
            Str, NumCUs, Error, NullCharError, !IO)
    else
        error("io.read_file_as_string: buffer overflow")
    ).

%---------------------%

read_binary_file_as_bitmap_2(Stream, Result, !IO) :-
    % Check if the stream is a regular file; if so, allocate a buffer
    % according to the size of the file. Otherwise, just use a default buffer
    % size of 4k minus a bit (to give malloc some room).
    binary_input_stream_file_size(Stream, FileSize, !IO),
    ( if FileSize >= 0i64 then
        binary_input_stream_offset64(Stream, CurrentOffset, !IO),
        RemainingSizeInt64 = FileSize - CurrentOffset,
        ( if
            int.bits_per_int = 32,
            RemainingSizeInt64 > int64.from_int(int.max_int)
        then
            Result = error(io_error_string("io.read_binary_file_as_bitmap: " ++
                "file size exceeds maximum buffer size"))
        else
            RemainingSize = int64.cast_to_int(RemainingSizeInt64),
            some [!BM] (
                !:BM = bitmap.init(RemainingSize * bits_per_byte),
                ( if RemainingSize = 0 then
                    Result = ok(!.BM)
                else
                    bitmap.read_bitmap_range(Stream, 0, RemainingSize, !BM,
                        BytesRead, ReadResult, !IO),
                    (
                        ReadResult = ok,
                        ( if BytesRead = RemainingSize then
                            Result = ok(!.BM)
                        else
                            Result = error(io_error_string(
                                "io.read_binary_file_as_bitmap: " ++
                                "incorrect file size"))
                        )
                    ;
                        ReadResult = error(Msg),
                        Result = error(Msg)
                    )
                )
            )
        )
    else
        BufferSize = 4000,
        read_binary_file_as_bitmap_from_stream(Stream, BufferSize,
            Res, [], RevBitmaps, !IO),
        (
            Res = ok,
            Result = ok(bitmap.append_list(reverse(RevBitmaps)))
        ;
            Res = error(Msg),
            Result = error(Msg)
        )
    ).

:- pred read_binary_file_as_bitmap_from_stream(io.binary_input_stream::in,
    num_bytes::in, io.res::out, list(bitmap)::in, list(bitmap)::out,
    io::di, io::uo) is det.

read_binary_file_as_bitmap_from_stream(Stream, BufferSize, Res, !BMs, !IO) :-
    some [!BM] (
        !:BM = bitmap.init(BufferSize * bits_per_byte),
        bitmap.read_bitmap_range(Stream, 0, BufferSize, !BM, NumBytesRead,
            ReadRes, !IO),
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
                read_binary_file_as_bitmap_from_stream(Stream, BufferSize * 2,
                    Res, !BMs, !IO)
            )
        ;
            ReadRes = error(Err),
            Res = error(Err)
        )
    ).

%---------------------%

    % XXX FIXME this should return an int64.
:- pred input_stream_file_size(io.text_input_stream::in, int::out,
    io::di, io::uo) is det.
:- pragma consider_used(pred(input_stream_file_size/4)).

input_stream_file_size(text_input_stream(Stream), Size, !IO) :-
    stream_file_size(Stream, Size64, !IO),
    Size = int64.cast_to_int(Size64).

:- pred binary_input_stream_file_size(io.binary_input_stream::in, int64::out,
    io::di, io::uo) is det.

binary_input_stream_file_size(binary_input_stream(Stream), Size, !IO) :-
    stream_file_size(Stream, Size, !IO).

    % stream_file_size(Stream, Size):
    %
    % If Stream is a regular file, then set Size to its size (in bytes),
    % otherwise set Size to -1.
    %
:- pred stream_file_size(stream::in, int64::out, io::di, io::uo) is det.

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
        Size = Stream.stream.Length;
    } else {
        Size = -1;
    }
}").

:- pragma foreign_proc("Java",
    stream_file_size(Stream::in, Size::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Size = ((jmercury.io__stream_ops.MR_BinaryFile) Stream).size();
    } catch (java.io.IOException e) {
        Size = -1;
    }
").

%---------------------%

% A buffer is an array of chars.
% For C backends, it is a C array of C chars.
% For other backends, it is a Mercury array of Mercury chars.

    % XXX It would be better to use a char_array type rather than array(char).
    % This is because on the Java (and maybe the C#) backend, indexing into
    % an array whose element type is known statically requires less overhead.
    %
    % It may be possible to merge with string.string_buffer.
    %
:- type buffer
    --->    buffer(array(char)).

:- pragma foreign_type(c, buffer, "char *", [can_pass_as_mercury_type]).

    % XXX Extend the workaround for no `ui' modes in array.m.
:- inst uniq_buffer for buffer/0
    --->    buffer(uniq_array).

:- mode buffer_di == di(uniq_buffer).
:- mode buffer_uo == out(uniq_buffer).

:- pred alloc_buffer(int::in, buffer::buffer_uo) is det.
:- pragma consider_used(pred(alloc_buffer/2)).

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
        // Just have to alloc and copy.
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

:- pred buffer_and_pos_to_string_and_length(buffer::buffer_di, int::in,
    string::out, int::out) is semidet.
% This predicate is used when compiling to C and C#; this pragma avoids
% a warning when compiling to Java.
:- pragma consider_used(pred(buffer_and_pos_to_string_and_length/4)).

:- pragma foreign_proc("C",
    buffer_and_pos_to_string_and_length(Buffer::buffer_di, Pos::in,
        Str::out, NumCUs::out),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"{
    Str = Buffer;
    Str[Pos] = '\\0';

    // Check that the string does not contain null characters.
    if (strlen(Str) != Pos) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
    }

    // In C, Pos counts bytes, which are the same size as UTF-8 code units.
    // NumCUs is expected to be in the code units native to the target
    // language, and this is UTF-8, so no conversion needs to be done.
    // (Compare to the C# case below.)
    NumCUs = Pos;
}").

buffer_and_pos_to_string_and_length(buffer(Array), Pos, Str, NumCUs) :-
    % This predicate is used only when compiling to C and C#, and when
    % targeting C, we use the foreign_proc above, so this clause is used
    % only when targeting C#.
    %
    % In C#, Pos counts chars, i.e. code points. Most code points occupy
    % just one UTF-16 code unit, but some occupy two. The call below to
    % semidet_from_char_list will do this expansion as necessary.
    % We can't know how many code units the final string contains
    % until we count them. (Compare to the C case above.)
    %
    % XXX The current implementation of read_file_as_string_2
    % reads in code units one by one, converts them to code points
    % to store them in array slots, then converts the array to a string,
    % which converts each code point back into one or two UTF-16 code units.
    % A fully C#-specific implementation of read_file_as_string_2,
    % one not shared with C, should be able to dispense with all the
    % redundant conversions.
    array.fetch_items(Array, min(Array), min(Array) + Pos - 1, List),
    string.semidet_from_char_list(List, Str),
    string.length(Str, NumCUs).

:- pred read_into_buffer(stream::in, buffer::buffer_di, buffer::buffer_uo,
    int::in, int::in, int::out, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_into_buffer(Stream::in, Buffer0::buffer_di, Buffer::buffer_uo,
        BufferSize::in, Pos0::in, Pos::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness],
"
    size_t bytes_to_read;
    size_t bytes_read;

    MR_CHECK_EXPR_TYPE(Buffer0, char *);
    MR_CHECK_EXPR_TYPE(Buffer, char *);

    bytes_to_read = BufferSize - Pos0;
    bytes_read = MR_READ(*Stream, Buffer0 + Pos0, bytes_to_read);

    Buffer = Buffer0;
    Pos = Pos0 + bytes_read;

    if (bytes_read < bytes_to_read && MR_FERROR(*Stream)) {
        Error = errno;
    } else {
        Error = 0;
    }
").

read_into_buffer(Stream, buffer(Array0), buffer(Array), BufferSize,
        !Pos, Error, !IO) :-
    % This predicate is used only when compiling to C and C#, and when
    % targeting C, we use the foreign_proc above, so this clause is used
    % only when targeting C#.
    read_into_array(text_input_stream(Stream), Array0, Array, BufferSize,
        !Pos, Error, !IO).

:- pred read_into_array(io.text_input_stream::in,
    array(char)::array_di, array(char)::array_uo, int::in, int::in, int::out,
    system_error::out, io::di, io::uo) is det.
% This predicate is not used when compiling to C or Java; this pragma avoids
% a warning even in those cases.
:- pragma consider_used(pred(read_into_array/9)).

read_into_array(Stream, !Array, ArraySize, !Pos, Error, !IO) :-
    ( if !.Pos >= ArraySize then
        Error = no_error
    else
        read_char_code(Stream, ResultCode, Error0, Char, !IO),
        (
            ResultCode = result_code_ok,
            array.set(!.Pos, Char, !Array),
            !:Pos = !.Pos + 1,
            read_into_array(Stream, !Array, ArraySize, !Pos, Error, !IO)
        ;
            ( ResultCode = result_code_eof
            ; ResultCode = result_code_error
            ),
            Error = Error0
        )
    ).

%---------------------------------------------------------------------------%
:- end_module io.text_read.
%---------------------------------------------------------------------------%
