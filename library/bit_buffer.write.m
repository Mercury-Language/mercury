%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2011 The University of Melbourne
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
% File: bit_buffer.write.m.
% Main author: stayl.
% Stability: low.
%
% A bit buffer provides an interface between bit-oriented output requests
% and byte-array-oriented streams, storing bits until there are enough bytes
% to make calling the `put' method on the stream worthwhile.
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

:- module bit_buffer.write.
:- interface.

:- import_module io.

:- type write_buffer(Stream, State).
        % <= stream.writer(Stream, bitmap.slice, State).

:- type write_buffer == write_buffer(error_stream, error_state).
:- type io_write_buffer == write_buffer(io.binary_output_stream, io.state).

:- inst uniq_write_buffer == ground.   % XXX Should be unique.
:- mode write_buffer_di == in(uniq_write_buffer).
:- mode write_buffer_ui == in(uniq_write_buffer).
:- mode write_buffer_uo == out(uniq_write_buffer).

    % new(NumBytes, Stream, State) creates a buffer which will write to
    % the stream specified by Stream and State in chunks of NumBytes bytes.
    % If NumBytes is less than the size of an integer (given by
    % int.bits_per_int), the size of an integer will be used instead.
    %
:- func new(num_bytes, Stream, State) = write_buffer(Stream, State)
    <= stream.writer(Stream, byte_index, State).
:- mode new(in, in, di) = write_buffer_uo is det.

    % new(NumBytes):
    %
    % Create a buffer which collects all of the bits written, and does
    % not write them to a stream.  The bits are collected in chunks of
    % size NumBytes bytes, and are written to a bitmap by
    % `finalize_to_bitmap/1'.
    %
:- func new_bitmap_builder(num_bytes) = write_buffer.
:- mode new_bitmap_builder(in) = out is det.

    % How many bits to be written does the buffer contain?
    %
:- func num_buffered_bits(write_buffer(_, _)) = num_bits.
:- mode num_buffered_bits(write_buffer_ui) = out is det.

    % Return how many bits need to be written to get to a byte boundary
    % in the output stream.
    %
:- func num_bits_to_byte_boundary(write_buffer(_, _)) = num_bits.
:- mode num_bits_to_byte_boundary(write_buffer_ui) = out is det.

    % Write a bit to the buffer.
    %
:- pred put_bit(bool, write_buffer(Stream, State), write_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode put_bit(in, write_buffer_di, write_buffer_uo) is det.

    % Write the given number of low-order bits from an int to the buffer.
    % The number of bits must be less than int.bits_per_int.
    %
:- pred put_bits(word, num_bits, write_buffer(Stream, State),
    write_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode put_bits(in, in, write_buffer_di, write_buffer_uo) is det.

    % Write the eight low-order bits from an int to the buffer.
    % The number of bits must be less than int.bits_per_int.
    %
:- pred put_byte(word, write_buffer(Stream, State),
    write_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode put_byte(in, write_buffer_di, write_buffer_uo) is det.

    % Write bits from a bitmap to the buffer.
    % The buffer does not keep a reference to the bitmap.
    %
:- pred put_bitmap(bitmap, write_buffer(Stream, State),
    write_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode put_bitmap(bitmap_ui, write_buffer_di, write_buffer_uo) is det.

:- pred put_bitmap(bitmap, bit_index, num_bits,
    write_buffer(Stream, State), write_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode put_bitmap(bitmap_ui, in, in, write_buffer_di, write_buffer_uo) is det.

    % Flush all complete bytes in the buffer to the output stream.
    % If there is an incomplete final byte it will remain unwritten
    % in the buffer.
    %
:- pred flush(write_buffer(Stream, State), write_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode flush(write_buffer_di, write_buffer_uo) is det.

    % Pad the buffered data out to a byte boundary, flush it to
    % the output stream, then return the Stream and State.
    %
:- pred finalize(write_buffer(Stream, State), Stream, State)
    <= stream.writer(Stream, bitmap.slice, State).
:- mode finalize(write_buffer_di, out, uo) is det.

    % Copy the data from a non-streamed write_buffer to a bitmap.
    % The output is not padded to an even number of bits.
    %
:- func finalize_to_bitmap(write_buffer) = bitmap.
:- mode finalize_to_bitmap(write_buffer_di) = bitmap_uo is det.

%---------------------------------------------------------------------------%
:- implementation.

/*
:- interface.
    % A write_buffer can be used as an output stream.
    % XXX These instances don't work because of the duplicated variables
    % in the head.
    %
:- type write_stream(Stream) ---> write_stream.
:- instance stream.stream(write_stream(Stream), write_buffer(Stream, State))
            <= stream.stream(Stream, State).
:- instance stream.output(write_stream(Stream), write_buffer(Stream, State))
            <= stream.writer(Stream, bitmap.slice, State).
:- instance stream.writer(write_stream(Stream), bool,
                write_buffer(Stream, State))
            <= stream.writer(Stream, bitmap.slice, State).
:- instance stream.writer(write_stream(Stream), bitmap.slice,
                write_buffer(Stream, State))
            <= stream.writer(Stream, bitmap.slice, State).
*/

    % For a write_buffer, a bit_buffer is allocated that is bits_per_int
    % larger than the size requested. This allows a full word to be
    % written at any time. After each write, the position is checked.
    % If the position is greater than the requested size, a chunk of the
    % requested size is written to the stream. The unwritten bits are
    % then copied to the start of the buffer.
    %
    % We always use a buffer size that is at least the size of a word
    % so that writing a word to the buffer will require at most a single
    % call to `put'. Allowing smaller sizes complicates the code
    % for a case that shouldn't occur in practice.
    %
    % For a bitmap_builder, we store the filled bitmaps in a list rather
    % than writing them to an output stream.
    %
:- type write_buffer(Stream, State)
    --->    write_buffer(
                bit_buffer :: bit_buffer(Stream, State)
            ).

new(NumBytes, Stream, State) = Buffer :-
    SizeInBits = NumBytes * bits_per_byte,
    Size = int.max(SizeInBits, bits_per_int),
    BM = bitmap.init(Size + int.bits_per_int, no),
    Buffer = write_buffer(new_buffer(BM, 0, Size, yes, Stream, State)).

new_bitmap_builder(NumBytes) = Buffer :-
    Size = NumBytes * bits_per_byte,
    BM = bitmap.init(Size + int.bits_per_int, no),
    Buffer = write_buffer(new_buffer(BM, 0, Size, no,
        error_stream, error_state)).

num_buffered_bits(write_buffer(Buffer)) =
    Buffer ^ pos +
        foldl((func(BM, N) = N + BM ^ num_bits), Buffer ^ filled_bitmaps, 0).

num_bits_to_byte_boundary(Buffer) = NumBits :-
    Pos = Buffer ^ bit_buffer ^ pos,
    Rem = Pos `unchecked_rem` bits_per_byte,
    ( if Rem = 0 then
        NumBits = 0
    else
        NumBits = bits_per_byte - Rem
    ).

put_bit(yes, !Buffer) :-
    put_bits(1, 1, !Buffer).
put_bit(no, !Buffer) :-
    put_bits(0, 1, !Buffer).

put_bits(Bits, NumBits, write_buffer(!.Buffer), write_buffer(!:Buffer)) :-
    BM0 = !.Buffer ^ bitmap,
    Pos0 = !.Buffer ^ pos,

    % We always make sure there is enough room in the buffer for a full
    % word to be written, so this can't run off the end of the bitmap.
    %
    BM = BM0 ^ bits(Pos0, NumBits) := Bits,
    Pos = Pos0 + NumBits,
    set_bitmap(BM, Pos, !Buffer),
    maybe_make_room(!Buffer).

put_byte(Byte, !Buffer) :-
    put_bits(Byte, bits_per_byte, !Buffer).

put_bitmap(BM, !Buffer) :-
    put_bitmap(BM, 0, BM ^ num_bits, !Buffer).

put_bitmap(BM, Index, NumBits,
        write_buffer(!.Buffer), write_buffer(!:Buffer)) :-
    put_bitmap_2(BM, Index, NumBits, !Buffer).

    % XXX If we're writing to a list of bitmaps and the user doesn't want
    % to write to the bitmap again, we should just add the bitmap passed
    % by the user to the list of filled bitmaps, if the current buffer
    % bitmap is full enough that we're not wasting too much space.
    %
:- pred put_bitmap_2(bitmap, bit_index, num_bits,
    bit_buffer(Stream, State), bit_buffer(Stream, State))
    <= stream.writer(Stream, bitmap.slice, State).
:- mode put_bitmap_2(bitmap_ui, in, in,
    write_buffer_di, write_buffer_uo) is det.

put_bitmap_2(BM, Index, NumBits, !Buffer) :-
    ( if NumBits = 0 then
        true
    else
        BufferBM0 = !.Buffer ^ bitmap,
        Pos = !.Buffer ^ pos,
        Size = !.Buffer ^ size,
        Remain = Size - Pos,
        NumBitsToWrite = int.min(Remain, NumBits),
        BufferBM = copy_bits(BM, Index, BufferBM0, Pos, NumBitsToWrite),
        set_bitmap(BufferBM, Pos + NumBitsToWrite, !Buffer),
        maybe_make_room(!Buffer),
        put_bitmap_2(BM, Index + NumBitsToWrite,
            NumBits - NumBitsToWrite, !Buffer)
    ).

flush(write_buffer(!.Buffer), write_buffer(!:Buffer)) :-
    UseStream = !.Buffer ^ use_stream,
    (
        UseStream = yes,
        flush_all_to_stream(!Buffer)
    ;
        UseStream = no
    ).

:- pred flush_all_to_stream(bit_buffer(Stream, State)::bit_buffer_di,
    bit_buffer(Stream, State)::bit_buffer_uo) is det
    <= stream.writer(Stream, bitmap.slice, State).

flush_all_to_stream(!Buffer) :-
    ( if num_buffered_bits(write_buffer(!.Buffer)) >= bits_per_byte then
        flush_chunk_to_stream(!Buffer),
        flush_all_to_stream(!Buffer)
    else
        true
    ).

:- pred flush_chunk_to_stream(bit_buffer(Stream, State)::bit_buffer_di,
    bit_buffer(Stream, State)::bit_buffer_uo) is det
    <= stream.writer(Stream, bitmap.slice, State).

flush_chunk_to_stream(!Buffer) :-
    % Write at most Size bytes at once (this is the output chunk size
    % set in the call to `new').
    Pos = !.Buffer ^ pos,
    Size = !.Buffer ^ size,
    NumBitsToWrite0 = int.min(Size, Pos),
    NumBytes = NumBitsToWrite0 `unchecked_quotient` bits_per_byte,
    ( if NumBytes = 0 then
        true
    else
        NumBitsToWrite = NumBytes * bits_per_byte,
        stream.put(!.Buffer ^ stream,
            bitmap.byte_slice(!.Buffer ^ bitmap, 0, NumBytes),
            unsafe_promise_unique(!.Buffer ^ state), NewState),
        Remain = Pos - NumBitsToWrite,
        ( if Remain = 0 then
            NewBM = !.Buffer ^ bitmap
        else
            % Copy the remainder to the start of the bitmap.
            % (We know that there are at most int.bits_per_int bits
            % after the flush because that was the size of the bitmap
            % created in `new', so we don't need to use copy_bits here).
            NewBM0 = !.Buffer ^ bitmap,
            NewBM = NewBM0 ^ bits(0, Remain) :=
                NewBM0 ^ bits(NumBitsToWrite, Remain)
        ),
        set_all(NewBM, Remain, !.Buffer ^ size, NewState,
            !.Buffer ^ filled_bitmaps, !Buffer)
    ).

finalize(!.Buffer, Stream, State) :-
    BitsToByte = num_bits_to_byte_boundary(!.Buffer),
    put_bits(0, BitsToByte, !Buffer),
    flush(!Buffer),
    Stream = !.Buffer ^ bit_buffer ^ stream,
    State = unsafe_promise_unique(!.Buffer ^ bit_buffer ^ state).

finalize_to_bitmap(write_buffer(Buffer)) = !:BM :-
    NumBits = num_buffered_bits(write_buffer(Buffer)),
    !:BM = bitmap.init(NumBits),

    % Copy out the filled bitmaps starting at the end of the result bitmap.
    %
    LastBM = shrink_without_copying(Buffer ^ bitmap, Buffer ^ pos),
    copy_out_bitmap(LastBM, NumBits, Index, !BM),
    list.foldl2(copy_out_bitmap, Buffer ^ filled_bitmaps, Index, _, !BM).

    % Copy the bitmap to the result bitmap, starting at the end.
    %
:- pred copy_out_bitmap(bitmap::in, bit_index::in,
    bit_index::out, bitmap::bitmap_di, bitmap::bitmap_uo) is det.

copy_out_bitmap(FilledBM, !Index, !BM) :-
    Size = FilledBM ^ num_bits,
    ( if Size > 0 then
        !:Index = !.Index - Size,
        !:BM = bitmap.copy_bits(FilledBM, 0, !.BM, !.Index, Size)
    else
        true
    ).

:- pred maybe_make_room(bit_buffer(Stream, State)::bit_buffer_di,
    bit_buffer(Stream, State)::bit_buffer_uo) is det
    <= stream.writer(Stream, bitmap.slice, State).

maybe_make_room(!Buffer) :-
    ( if !.Buffer ^ pos >= !.Buffer ^ size then
        make_room(!Buffer)
    else
        true
    ).

:- pred make_room(bit_buffer(Stream, State)::bit_buffer_di,
    bit_buffer(Stream, State)::bit_buffer_uo) is det
    <= stream.writer(Stream, bitmap.slice, State).

make_room(!Buffer) :-
    UseStream = !.Buffer ^ use_stream,
    (
        UseStream = yes,
        flush_chunk_to_stream(!Buffer)
    ;
        UseStream = no,
        store_full_buffer(!Buffer)
    ).

    % This must only be called when the buffer has less than a word
    % of space left.
    %
:- pred store_full_buffer(bit_buffer(Stream, State)::in,
    bit_buffer(Stream, State)::out) is det
    <= stream.writer(Stream, bitmap.slice, State).

store_full_buffer(!Buffer) :-
    Pos = !.Buffer ^ pos,
    Size = !.Buffer ^ size,
    OldBM = !.Buffer ^ bitmap,
    State = !.Buffer ^ state,

    % Double the buffer size at each allocation.
    NewSize = (!.Buffer ^ size) * 2,

    % Create the new bitmap, copying the left-over bytes from the old one.
    % (We know that there are at most int.bits_per_int bits after the flush
    % because that was the size of the bitmap created in `new', so
    % we don't need to use copy_bits here).
    NewBM0 = bitmap.init(NewSize + int.bits_per_int),
    Remain = Pos - Size,
    NewPos = Remain,
    NewBM = NewBM0 ^ bits(0, Remain) := OldBM ^ bits(Size, Remain),
    FilledBM = shrink_without_copying(OldBM, Size),

    FilledBMs = [FilledBM | !.Buffer ^ filled_bitmaps],

    set_all(NewBM, NewPos, NewSize,
        unsafe_promise_unique(State), FilledBMs, !Buffer).

%---------------------------------------------------------------------------%

/*
** XXX These instances don't work because of duplicated head variables.

:- instance stream.stream(write_stream(Stream), write_buffer(Stream, State))
        <= stream.stream(Stream, State)
    where
[
    (name(_, Name, !Buffer) :-
        BitBuffer0 = !.Buffer ^ bit_buffer,
        State0 = BitBuffer0 ^ state,
        Stream = BitBuffer0 ^ stream,
        name(Stream, StreamName, State0, State),
        Name = "bit_buffer.write.write_buffer(" ++ StreamName ++ ")",
        set_state(State, BitBuffer0, BitBuffer),
        !:Buffer = write_buffer(BitBuffer)
    )
].
:- instance stream.output(write_stream(Stream), write_buffer(Stream, State))
        <= stream.writer(Stream, bitmap.slice, State)
    where
[
    (flush(_, !Buffer) :-
        flush(!Buffer)
    )
].
:- instance stream.writer(write_stream(Stream), bool,
            write_buffer(Stream, State))
        <= stream.writer(Stream, bitmap.slice, State)
    where
[
    (put(_, Bit, !Buffer) :- put_bit(Bit, !Buffer))
].
:- instance stream.writer(write_stream(Stream), bitmap.slice,
            write_buffer(Stream, State))
        <= stream.writer(Stream, bitmap.slice, State)
    where
[
    (put(_, Slice, !Buffer) :-
        put_bitmap(Slice ^ slice_bitmap, Slice ^ slice_start_bit_index,
            Slice ^ slice_num_bits, !Buffer)
    )
].
*/

%---------------------------------------------------------------------------%
:- end_module bit_buffer.write.
%---------------------------------------------------------------------------%
