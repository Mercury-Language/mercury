%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2009 The University of Melbourne
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
% File: bit_buffer.m.
% Main author: stayl.
% Stability: low.
%
% A bit buffer provides an interface between bit-oriented I/O requests
% and byte-oriented streams.  The useful part of the interface is defined
% in bit_buffer.read and bit_buffer.write.
%
% CAVEAT: the user is referred to the documentation in the header
% of array.m regarding programming with unique objects (the compiler
% does not currently recognise them, hence we are forced to use
% non-unique modes until the situation is rectified; this places
% a small burden on the programmer to ensure the correctness of his
% code that would otherwise be assured by the compiler.)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bit_buffer.
:- interface.

:- import_module bitmap.
:- import_module stream.

:- include_module bit_buffer.read.
:- include_module bit_buffer.write.

    % An error_stream throws an `error_stream_error' exception if any of
    % its output methods are called, or returns an `error_stream_error'
    % if any of its input methods are called.
    %
:- type error_stream ---> error_stream.
:- type error_state ---> error_state.
:- type error_stream_error ---> error_stream_error.
:- instance stream.error(error_stream_error).
:- instance stream.stream(error_stream, error_state).
:- instance stream.input(error_stream, error_state).
:- instance stream.bulk_reader(error_stream, byte_index, bitmap,
        error_state, error_stream_error).

:- instance stream.output(error_stream, error_state).
:- instance stream.writer(error_stream, bitmap.slice, error_state).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module list.

:- instance stream.error(error_stream_error) where
[
    error_message(_) = "method called for error_stream"
].

:- instance stream.stream(error_stream, error_state) where
[
    name(_Stream, "error_stream", !State)
].

:- instance stream.input(error_stream, error_state) where
[].

:- instance stream.bulk_reader(error_stream, byte_index, bitmap,
    error_state, error_stream_error) where
[
    bulk_get(_, _, _, !BM, 0, error(error_stream_error), !State)
].

:- instance stream.output(error_stream, error_state) where
[
    flush(_, !State) :- throw(error_stream_error)
].

:- instance stream.writer(error_stream, bitmap.slice, error_state)
    where
[
    put(_, _, _, _) :- throw(error_stream_error)
].

    % The bitmap has room for the chunk size given as an argument
    % to `new', plus a word.
    %
    % This means that a for a write buffer a word can always
    % be written to the buffer, and the buffer will be flushed
    % if the position is greater than the chunk size.
    %
    % For a read_buffer, bits will be read from the input stream
    % and placed starting at bit number `bits_per_int'.  When the
    % buffer is nearly exhausted (less than a word left), the last
    % word is copied to the start of the buffer and the buffer is
    % refilled.
    %
:- type bit_buffer(Stream, State, Error)
    --->    bit_buffer(
                mer_bitmap :: bitmap,
                mer_pos :: bit_index,
                mer_size :: num_bits,

                mer_use_stream :: bool,

                mer_stream :: Stream,
                mer_state :: State,

                % For write buffers only.
                % If we're not writing to a stream, keep a list of filled
                % bitmaps in reverse order. These will be concatenated into
                % a single bitmap by finalize_to_bitmap.
                %
                mer_filled_bitmaps :: list(bitmap),

                % For read buffers only. The first error found when reading
                % from a stream. Subsequent calls will return this error.
                %
                mer_read_status :: stream.res(Error)
            ).

:- type bit_buffer(Stream, State) == bit_buffer(Stream, State, {}).

    % XXX These should be unique.
:- mode bit_buffer_ui == in.
:- mode bit_buffer_di == in.
:- mode bit_buffer_uo == out.

    % Allocating memory for every read or write would be bad, so
    % we manually perform destructive update in C.
    %
:- pragma foreign_decl("C", "
typedef struct {
    MR_BitmapPtr    ML_bit_buffer_bitmap;
    MR_Integer      ML_bit_buffer_pos;
    MR_Integer      ML_bit_buffer_size;
    MR_Word         ML_bit_buffer_use_stream;
    MR_Word         ML_bit_buffer_stream;
    MR_Word         ML_bit_buffer_state;
    MR_Word         ML_bit_buffer_filled_bitmaps;
    MR_Word         ML_bit_buffer_read_status;
} ML_BitBuffer;

typedef ML_BitBuffer *ML_BitBufferPtr;
").

:- pragma foreign_type("C", bit_buffer(Stream, State, Error),
    "ML_BitBufferPtr", [can_pass_as_mercury_type]).

:- func new_buffer(bitmap, bit_index, num_bits, bool, Stream, State) =
    bit_buffer(Stream, State, Error).

new_buffer(BM, Pos, Size, UseStream, Stream, State) =
    ( if Size =< 0 then
        throw("bit_buffer: invalid buffer size")
      else
        new_buffer_2(BM, Pos, Size, UseStream, Stream, State, ok)
    ).

:- func new_buffer_2(bitmap, num_bits, bit_index, bool,
    Stream, State, stream.res(Error)) = bit_buffer(Stream, State, Error).

new_buffer_2(BM, Pos, Size, UseStream, Stream, State, ReadStatus) =
    bit_buffer(BM, Pos, Size, UseStream, Stream, State, [], ReadStatus).

:- pragma foreign_proc("C",
    new_buffer_2(BM::in, Pos::in, Size::in, UseStream::in,
        Stream::in, State::in, ReadStatus::in) = (Buffer::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    MR_incr_hp_type_msg(Buffer, ML_BitBuffer, MR_ALLOC_ID,
        ""bit_buffer.bit_buffer/3"");
    Buffer->ML_bit_buffer_bitmap = BM;
    Buffer->ML_bit_buffer_pos = Pos;
    Buffer->ML_bit_buffer_size = Size;
    Buffer->ML_bit_buffer_use_stream = UseStream;
    Buffer->ML_bit_buffer_stream = Stream;
    Buffer->ML_bit_buffer_state = State;
    Buffer->ML_bit_buffer_filled_bitmaps = MR_list_empty_msg(MR_ALLOC_ID);
    Buffer->ML_bit_buffer_read_status = ReadStatus;
}").

:- func (bit_buffer(_, _, _)::bit_buffer_ui) ^ bitmap =
    (bitmap::bitmap_uo) is det.
:- func (bit_buffer(_, _, _)::bit_buffer_ui) ^ pos = (int::out) is det.
:- func (bit_buffer(_, _, _)::bit_buffer_ui) ^ size = (int::out) is det.
:- func (bit_buffer(_, _, _)::bit_buffer_ui) ^ use_stream = (bool::out) is det.
:- func (bit_buffer(Stream, _, _)::bit_buffer_ui) ^ stream =
    (Stream::out) is det.
:- func (bit_buffer(_, State, _)::bit_buffer_ui) ^ state = (State::uo) is det.
:- func (bit_buffer(_, _, _)::bit_buffer_ui) ^ filled_bitmaps =
    (list(bitmap)::out) is det.
:- func (bit_buffer(_, _, Error)::bit_buffer_ui) ^ read_status =
    (stream.res(Error)::out) is det.

Buffer ^ bitmap = Buffer ^ mer_bitmap.
Buffer ^ pos = Buffer ^ mer_pos.
Buffer ^ size = Buffer ^ mer_size.
Buffer ^ use_stream = Buffer ^ mer_use_stream.
Buffer ^ stream = Buffer ^ mer_stream.
Buffer ^ state = unsafe_promise_unique(Buffer ^ mer_state).
Buffer ^ filled_bitmaps = Buffer ^ mer_filled_bitmaps.
Buffer ^ read_status = Buffer ^ mer_read_status.

:- pragma foreign_proc("C",
    bitmap(Buffer::bit_buffer_ui) = (BM::bitmap_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BM = Buffer->ML_bit_buffer_bitmap;
").

:- pragma foreign_proc("C",
    pos(Buffer::bit_buffer_ui) = (Pos::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pos = Buffer->ML_bit_buffer_pos;
").

:- pragma foreign_proc("C",
    size(Buffer::bit_buffer_ui) = (Size::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Size = Buffer->ML_bit_buffer_size;
").

:- pragma foreign_proc("C",
    use_stream(Buffer::bit_buffer_ui) = (UseStream::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    UseStream = Buffer->ML_bit_buffer_use_stream;
").

:- pragma foreign_proc("C",
    stream(Buffer::bit_buffer_ui) = (Stream::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Stream = Buffer->ML_bit_buffer_stream;
").

:- pragma foreign_proc("C",
    state(Buffer::bit_buffer_ui) = (State::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    State = Buffer->ML_bit_buffer_state;
").

:- pragma foreign_proc("C",
    filled_bitmaps(Buffer::bit_buffer_ui) = (FilledBMs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FilledBMs = Buffer->ML_bit_buffer_filled_bitmaps;
").

:- pragma foreign_proc("C",
    read_status(Buffer::bit_buffer_ui) = (ReadStatus::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ReadStatus = Buffer->ML_bit_buffer_read_status;
").

:- pred set_all(bitmap::bitmap_di, bit_index::in, num_bits::in, State::di,
    list(bitmap)::in, bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det.

set_all(BM, Pos, Size, State, FilledBMs, !Buffer) :-
    !Buffer ^ mer_bitmap := BM,
    !Buffer ^ mer_pos := Pos,
    !Buffer ^ mer_state :=  State,
    !Buffer ^ mer_filled_bitmaps := FilledBMs,
    !Buffer ^ mer_size := Size.

:- pragma foreign_proc("C",
    set_all(BM::bitmap_di, Pos::in, Size::in, State::di, FilledBMs::in,
        Buffer0::bit_buffer_di, Buffer::bit_buffer_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = Buffer0;
    Buffer->ML_bit_buffer_bitmap = BM;
    Buffer->ML_bit_buffer_pos = Pos;
    Buffer->ML_bit_buffer_size = Size;
    Buffer->ML_bit_buffer_state = State;
    Buffer->ML_bit_buffer_filled_bitmaps = FilledBMs;
").

:- pred set_bitmap(bitmap::bitmap_di, bit_index::in,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det.

set_bitmap(BM, Pos, !Buffer) :-
    !Buffer ^ mer_bitmap := BM,
    !Buffer ^ mer_pos := Pos.

:- pragma foreign_proc("C",
    set_bitmap(BM::bitmap_di, Pos::in,
        Buffer0::bit_buffer_di, Buffer::bit_buffer_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = Buffer0;
    Buffer->ML_bit_buffer_bitmap = BM;
    Buffer->ML_bit_buffer_pos = Pos;
").

:- pred set_state(State::di,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det.

set_state(State, !Buffer) :-
    !:Buffer = !.Buffer ^ mer_state := State.

:- pragma foreign_proc("C",
    set_state(State::di, Buffer0::bit_buffer_di, Buffer::bit_buffer_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = Buffer0;
    Buffer->ML_bit_buffer_state = State;
").

:- pred set_use_stream(bool::in,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det.

set_use_stream(UseStream, !Buffer) :-
    !:Buffer = !.Buffer ^ mer_use_stream := UseStream.

:- pragma foreign_proc("C",
    set_use_stream(UseStream::in,
        Buffer0::bit_buffer_di, Buffer::bit_buffer_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = Buffer0;
    Buffer->ML_bit_buffer_use_stream = UseStream;
").

:- pred set_read_status(stream.res(Error)::in,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det.

set_read_status(ReadStatus, !Buffer) :-
    !:Buffer = !.Buffer ^ mer_read_status := ReadStatus.

:- pragma foreign_proc("C",
    set_read_status(ReadStatus::in,
        Buffer0::bit_buffer_di, Buffer::bit_buffer_uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buffer = Buffer0;
    Buffer->ML_bit_buffer_read_status = ReadStatus;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
