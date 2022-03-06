%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2010-2011 The University of Melbourne
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
% File: bit_buffer.read.m.
% Main author: stayl.
% Stability: low.
%
% A bit buffer provides an interface between bit-oriented input requests
% and byte-oriented streams, getting a large chunk of bits with one call
% to `bulk_get', then satisfying bit-oriented requests from the buffer.
%
% Return values of `error(...)' are only used for errors in the stream
% being read.  Once an error value has been returned, all future calls
% will return that error.
%
% Bounds errors or invalid argument errors (for example a read request
% for a negative number of bits) will result in an exception being thrown.
% Requests triggering an exception in this way will not change the state
% of the stream.
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

:- module bit_buffer.read.
:- interface.

:- import_module io.
:- import_module bitmap.

:- type read_buffer(Stream, State, Error).
    % <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).

:- type read_buffer ==
    read_buffer(error_stream, error_state, error_stream_error).

:- type io_read_buffer ==
    read_buffer(io.binary_input_stream, io.state, io.error).

:- inst uniq_read_buffer == ground.   % XXX Should be unique.
:- mode read_buffer_di == in(uniq_read_buffer).
:- mode read_buffer_ui == in(uniq_read_buffer).
:- mode read_buffer_uo == out(uniq_read_buffer).

    % new(NumBytes, Stream, State) creates a buffer which will read from
    % the stream specified by Stream and State in chunks of NumBytes bytes.
    % `NumBytes' must at least the size of a Mercury int, given by
    % int.bits_per_int.  If it is less, the size of an int will be used
    % instead.
    %
:- func new(num_bytes, Stream, State) = read_buffer(Stream, State, Error)
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode new(in, in, di) = read_buffer_uo is det.

    % new(BitIndex, StartIndex, NumBits)
    % Create a buffer which reads bits from a bitmap, not from a stream.
    %
:- func new_bitmap_reader(bitmap, bit_index, num_bits) = read_buffer.
:- mode new_bitmap_reader(in, in, in) = read_buffer_uo is det.

:- func new_bitmap_reader(bitmap) = read_buffer.
:- mode new_bitmap_reader(in) = read_buffer_uo is det.

    % How many bits to be read does the buffer contain.
    %
:- func num_buffered_bits(read_buffer(_, _, _)) = num_bits.
:- mode num_buffered_bits(read_buffer_ui) = out is det.

    % How many bits need to be read to get to the next byte boundary.
    %
:- func num_bits_to_byte_boundary(read_buffer(_, _, _)) = num_bits.
:- mode num_bits_to_byte_boundary(read_buffer_ui) = out is det.

    % Find out whether there are bits left in the stream or an error
    % has been found.
    %
:- pred buffer_status(stream.result(Error),
    read_buffer(Stream, State, Error),
    read_buffer(Stream, State, Error))
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode buffer_status(out, read_buffer_di, read_buffer_uo) is det.

    % Read a bit from the buffer.
    %
    % This implements the get/4 method of class stream.reader.
    %
:- pred get_bit(stream.result(bool, Error), read_buffer(Stream, State, Error),
    read_buffer(Stream, State, Error))
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode get_bit(out, read_buffer_di, read_buffer_uo) is det.

    % get_bits(Index, NumBits, !Word, NumBitsRead, Result, !Buffer).
    %
    % Read NumBits bits from the buffer into a word starting at Index,
    % where the highest order bit is bit zero.
    % 0 =< NumBits =< int.bits_per_int.
    %
    % This implements the bulk_get/9 method of stream.bulk_reader.
    %
    % To read into the lower order bits of the word, use
    % `get_bits(bits_per_int - NumBits, NumBits, ...)'.
    %
:- pred get_bits(bit_index, num_bits, word, word, num_bits,
    stream.res(Error), read_buffer(Stream, State, Error),
    read_buffer(Stream, State, Error))
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode get_bits(in, in, di, uo, out, out,
    read_buffer_di, read_buffer_uo) is det.

    % get_bitmap(!Bitmap, NumBitsRead, Result, !Buffer)
    %
    % Fill a bitmap from the buffered stream, returning the number
    % of bits read.
    %
    % Note that this is much more efficient if the initial position in
    % the buffer is at a byte boundary (for example after a call to
    % skip_padding_to_byte).
    %
:- pred get_bitmap(bitmap, bitmap, num_bits,
    stream.res(Error), read_buffer(Stream, State, Error),
    read_buffer(Stream, State, Error))
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode get_bitmap(bitmap_di, bitmap_uo, out, out,
    read_buffer_di, read_buffer_uo) is det.

    % get_bitmap(Index, NumBits, !Bitmap, NumBitsRead, Result, !Buffer)
    %
    % Note that this is much more efficient if both Index and the initial
    % position in the buffer are both at a byte boundary (for example after
    % a call to skip_padding_to_byte).
    %
    % This implements the bulk_get method of stream.bulk_reader.
    %
:- pred get_bitmap(bit_index, num_bits, bitmap, bitmap, num_bits,
    stream.res(Error), read_buffer(Stream, State, Error),
    read_buffer(Stream, State, Error))
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode get_bitmap(in, in, bitmap_di, bitmap_uo, out, out,
    read_buffer_di, read_buffer_uo) is det.

    % finalize(Buffer, Stream, State, BufferBM,
    %   IndexInBufferBM, NumBitsInBufferBM)
    %
    % Returns the stream, state and the unread buffered bits.
    %
:- pred finalize(read_buffer(Stream, State, Error), Stream, State,
    bitmap, bit_index, num_bits)
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).
:- mode finalize(read_buffer_di, out, uo, bitmap_uo, out, out) is det.

%---------------------------------------------------------------------------%

% None of these instances work because of limitations in the type and
% RTTI systems.
%
% :- implementation.
% :- interface.
% 
%     % A bit buffer is a stream of bits.
%     %
% :- type read_stream(Stream)
%     ---> read_stream.
% 
% :- instance stream.stream(read_stream(Stream, Error),
%         read_buffer(Stream, State, Error))
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error).
% 
% :- instance stream.input(read_stream(Stream, Error),
%         read_buffer(Stream, State, Error))
%     <= stream.input(Stream, State).
% 
% :- instance stream.reader(read_stream(Stream, Error), bool,
%         read_buffer(Stream, State, Error), Error)
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error).
% 
% :- instance stream.bulk_reader(read_stream(Stream, Error),
%         bit_index, word, read_buffer(Stream, State, Error), Error)
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error).
% 
% :- instance stream.bulk_reader(read_stream(Stream, Error),
%         bit_index, bitmap, read_buffer(Stream, State, Error), Error)
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

    % If an operation reports an error, it must set the read_error
    % field so that all further operations report an error as well.
    % This is done at the site the error is discovered; places which
    % just convert the type of an error value don't need to set the
    % read_error field.
    %
:- type read_buffer(Stream, State, Error)
    ---> read_buffer(bit_buffer :: bit_buffer(Stream, State, Error)).
    % <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).

new(NumBytes, Stream, State) = Buffer :-
    % We store Size + bits_per_int bits in the buffer. The first word
    % of the buffer contains the bits that were in the buffer when it
    % was last refilled.
    %
    % We require the buffer size to be at least bits_per_int so that
    % a call to `get_bits' can always be satisfied with a single buffer refill.
    % Allowing smaller buffer sizes would complicate the code for a case
    % that shouldn't occur in practice anyway.
    %
    SizeInBits = NumBytes * bits_per_byte,
    ChunkSize = int.max(SizeInBits, bits_per_int),
    BMSize = ChunkSize + bits_per_int,
    BM = bitmap.init(BMSize, no),

    % Start at the end of the buffer to force a fill on the first read.
    %
    Pos = BMSize,
    Buffer = read_buffer(new_buffer(BM, Pos, BMSize, yes, Stream, State)).

new_bitmap_reader(BM, StartIndex, NumBits) = Buffer :-
    Buffer = read_buffer(new_buffer(BM, StartIndex, NumBits, no,
        error_stream, error_state)).

new_bitmap_reader(BM) = new_bitmap_reader(BM, 0, BM ^ num_bits).

num_buffered_bits(Buffer) =
    % The computed number of bits may be negative if there has been an error.
    int.max(Buffer ^ bit_buffer ^ size - Buffer ^ bit_buffer ^ pos, 0).

num_bits_to_byte_boundary(Buffer) = NumBits :-
    Pos = Buffer ^ bit_buffer ^ pos,
    PosInByte = Pos `unchecked_rem` bits_per_byte,
    ( if PosInByte = 0 then
        NumBits = 0
    else
        NumBits = bits_per_byte - PosInByte
    ).

buffer_status(Result, !Buffer) :-
    Status = !.Buffer ^ bit_buffer ^ read_status,
    (
        Status = ok,
        NumBufferedBits = !.Buffer ^ num_buffered_bits,
        ( if NumBufferedBits > 0 then
            Result = ok
        else
            refill_read_buffer(RefillResult, !Buffer),
            (
                RefillResult = ok,
                NewNumBufferedBits = !.Buffer ^ num_buffered_bits,
                ( if NewNumBufferedBits > 0 then
                    Result = ok
                else
                    Result = eof
                )
            ;
                RefillResult = error(Err),
                Result = error(Err)
            )
        )
    ;
        Status = error(Err),
        Result = error(Err)
    ).

get_bit(BitResult, !Buffer) :-
    get_bits(0, 1, 0, Word, NumBitsRead, BitsResult, !Buffer),
    (
        BitsResult = ok,
        ( if NumBitsRead = 1 then
            BitResult = ok(if Word = 0 then no else yes)
        else
            BitResult = eof
        )
    ;
        BitsResult = error(Error),
        BitResult = error(Error)
    ).

get_bits(Index, NumBits, !.Word, unsafe_promise_unique(!:Word),
        NumBitsRead, BitsResult, !Buffer) :-
    Status = !.Buffer ^ bit_buffer ^ read_status,
    (
        Status = ok,
        ( if NumBits > 0 then
            ( if NumBits > bits_per_int then
                unexpected($pred, "invalid number of bits")
            else
                true
            ),
            ( if !.Buffer ^ num_buffered_bits >= NumBits then
                BitsResult = ok,
                do_get_bits(Index, NumBits, !Word, NumBitsRead, !Buffer)
            else
                refill_read_buffer(RefillResult, !Buffer),
                (
                    RefillResult = ok,
                    BitsResult = ok,
                    do_get_bits(Index, NumBits, !Word, NumBitsRead, !Buffer)
                ;
                    RefillResult = error(Err),
                    NumBitsRead = 0,
                    BitsResult = error(Err)
                )
            )
        else if NumBits = 0 then
            NumBitsRead = 0,
            BitsResult = ok
        else
            unexpected($pred, "negative number of bits")
        )
    ;
        Status = error(Err),
        NumBitsRead = 0,
        BitsResult = error(Err)
    ).

:- pred do_get_bits(bit_index::in, num_bits::in, word::in, word::out,
    num_bits::out, read_buffer(Stream, State, Error)::read_buffer_di,
    read_buffer(Stream, State, Error)::read_buffer_uo) is det
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).

do_get_bits(Index, NumBits, !Word, NumBitsRead,
        RB @ read_buffer(!.Buffer), read_buffer(!:Buffer)) :-
    NumBitsAvailable = RB ^ num_buffered_bits,
    Pos = !.Buffer ^ pos,
    NumBitsRead = int.min(NumBitsAvailable, NumBits),
    Bits0 = !.Buffer ^ bitmap ^ bits(Pos, NumBitsRead),
    Bits = Bits0 `unchecked_left_shift` (NumBits - NumBitsRead),

    LastBit = Index + NumBitsRead - 1,
    Shift = bits_per_int - 1 - LastBit,
    BitMask = 1 `unchecked_left_shift` (NumBits - 1),
    BitsMask = BitMask \/ (BitMask - 1),
    !:Word = !.Word /\ \ (BitsMask `unchecked_left_shift` Shift),
    !:Word = !.Word \/ (Bits `unchecked_left_shift` Shift),

    set_bitmap(!.Buffer ^ bitmap, Pos + NumBits, !Buffer).

get_bitmap(!BM, NumBitsRead, Result, !Buffer) :-
    get_bitmap(0, !.BM ^ num_bits, !BM, NumBitsRead, Result, !Buffer).

get_bitmap(Index, NumBits, !BM, NumBitsRead, Result,
        read_buffer(!.Buffer), read_buffer(!:Buffer)) :-
    % This code is based on read_bitmap_range in bitmap.m.
    Status = !.Buffer ^ read_status,
    (
        Status = ok,
        ( if
            NumBits > 0,
            in_range(!.BM, Index),
            in_range(!.BM, Index + NumBits - 1)
        then
            UseStream = !.Buffer ^ use_stream,
            (
                UseStream = yes,
                recursively_get_bitmap(Index, NumBits, !BM, 0,
                    NumBitsRead, Result, !Buffer)
            ;
                UseStream = no,
                Pos = !.Buffer ^ pos,
                Size = !.Buffer ^ size,
                NumBitsRead = min(Size - Pos, NumBits),
                !:BM = copy_bits(!.Buffer ^ bitmap, Pos, !.BM,
                    Index, NumBitsRead),
                set_bitmap(!.Buffer ^ bitmap, Pos + NumBits, !Buffer),
                Result = ok
            )
        else if
            NumBits = 0,
            ( in_range(!.BM, Index)
            ; Index = 0
            )
        then
            NumBitsRead = 0,
            Result = ok
        else
            bitmap.throw_bounds_error(!.BM, "bit_buffer.read.get_bitmap",
                Index, NumBits)
        )
    ;
        Status = error(Error),
        NumBitsRead = 0,
        Result = error(Error)
    ).

:- pred recursively_get_bitmap(bit_index::in, num_bits::in,
    bitmap::bitmap_di, bitmap::bitmap_uo,
    num_bits::in, num_bits::out, stream.res(Error)::out,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).

recursively_get_bitmap(!.Index, !.NumBits, !BM, !NumBitsRead,
        Result, !Buffer) :-
    ( if !.NumBits = 0 then
        Result = ok
    else
        % Take the bits that are already in the buffer.
        copy_buffered_bits_to_bitmap(!Index, !NumBits, !BM,
            !NumBitsRead, !Buffer),
        ( if
            !.NumBits = 0
        then
            Result = ok
        else if
            !.Index `unchecked_rem` bits_per_byte = 0
        then
            % We can do a bulk_get straight into the result bitmap.
            bulk_get_into_result_bitmap(!Index, !NumBits, !BM, !NumBitsRead,
                BulkGetResult, !Buffer),
            (
                BulkGetResult = ok,
                ( if !.NumBits > 0 then
                    !:Buffer = read_buffer(!.Buffer),
                    get_bits(bits_per_int - !.NumBits, !.NumBits,
                        0, LastBits, NumLastBitsRead, LastBitsResult, !Buffer),
                    !:Buffer = !.Buffer ^ bit_buffer,
                    (
                        LastBitsResult = ok,

                        % !.NumBits is correct here, if we didn't read
                        % enough bits, this will just fill the rest of the
                        % range with zero bits.
                        !:BM = !.BM ^ bits(!.Index, !.NumBits) := LastBits,
                        Result = ok
                    ;
                        LastBitsResult = error(Err),
                        Result = error(Err)
                    ),
                    !:NumBitsRead = !.NumBitsRead + NumLastBitsRead
                else
                    Result = ok
                )
            ;
                BulkGetResult = error(Err),
                Result = error(Err)
            )
        else
            do_refill_read_buffer(RefillRes, !Buffer),
            (
                RefillRes = ok,
                ( if read_buffer(!.Buffer) ^ num_buffered_bits > 0 then
                    disable_warning [suspicious_recursion] (
                        recursively_get_bitmap(!.Index, !.NumBits, !BM,
                            !NumBitsRead, Result, !Buffer)
                    )
                else
                    Result = ok
                )
            ;
                RefillRes = error(Err),
                Result = error(Err)
            )
        )
    ).

:- pred copy_buffered_bits_to_bitmap(bit_index::in, bit_index::out,
    num_bits::in, num_bits::out, bitmap::bitmap_di, bitmap::bitmap_uo,
    num_bits::in, num_bits::out, bit_buffer(S, St, E)::bit_buffer_di,
    bit_buffer(S, St, E)::bit_buffer_uo) is det.

copy_buffered_bits_to_bitmap(!Index, !NumBits, !BM, !NumBitsRead, !Buffer) :-
    NumBufferedBits = read_buffer(!.Buffer) ^ num_buffered_bits,
    NumBitsToGet = int.min(!.NumBits, NumBufferedBits),
    Pos0 = !.Buffer ^ pos,
    !:BM = copy_bits(!.Buffer ^ bitmap, Pos0, !.BM, !.Index, NumBitsToGet),
    Pos = Pos0 + NumBitsToGet,
    set_bitmap(!.Buffer ^ bitmap, Pos, !Buffer),
    !:Index = !.Index + NumBitsToGet,
    !:NumBits = !.NumBits - NumBitsToGet,
    !:NumBitsRead = !.NumBitsRead + NumBitsToGet.

:- pred bulk_get_into_result_bitmap(bit_index::in, bit_index::out,
    num_bits::in, num_bits::out, bitmap::bitmap_di, bitmap::bitmap_uo,
    num_bits::in, num_bits::out, stream.res(E)::out,
    bit_buffer(S, St, E)::bit_buffer_di,
    bit_buffer(S, St, E)::bit_buffer_uo) is det
    <= stream.bulk_reader(S, byte_index, bitmap, St, E).

bulk_get_into_result_bitmap(!Index, !NumBits, !BM, !NumBitsRead,
        Result, !Buffer) :-
    StartByteIndex = !.Index `unchecked_quotient` bits_per_byte,
    NumBytesToBulkGet = !.NumBits `unchecked_quotient` bits_per_byte,
    Stream = !.Buffer ^ stream,
    State0 = !.Buffer ^ state,
    stream.bulk_get(Stream, StartByteIndex, NumBytesToBulkGet, !BM,
        NumBytesRead, Result, State0, State),
    (
        Result = ok
    ;
        Result = error(_),
        do_set_buffer_error(Result, !Buffer)
    ),
    NumBitsBulkRead = NumBytesRead * bits_per_byte,
    !:Index = !.Index + NumBitsBulkRead,
    !:NumBitsRead = !.NumBitsRead + NumBitsBulkRead,
    !:NumBits = !.NumBits - NumBitsBulkRead,
    set_state(State, !Buffer).

    % This predicate may only be called when the number of buffered bits
    % is less than bits_per_int.
    %
:- pred refill_read_buffer(stream.res(Error)::out,
    read_buffer(Stream, State, Error)::read_buffer_di,
    read_buffer(Stream, State, Error)::read_buffer_uo) is det
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).

refill_read_buffer(Result, read_buffer(!.Buffer), read_buffer(!:Buffer)) :-
    do_refill_read_buffer(Result, !Buffer).

:- pred do_refill_read_buffer(stream.res(Error)::out,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det
    <= stream.bulk_reader(Stream, byte_index, bitmap, State, Error).

do_refill_read_buffer(Result, !.Buffer, !:Buffer) :-
    UseStream = !.Buffer ^ use_stream,
    (
        UseStream = yes,
        ( if read_buffer(!.Buffer) ^ num_buffered_bits =< bits_per_int then
            true
        else
            unexpected($pred, "too many bits in buffer")
        ),
        some [!BM, !State, !Pos, !Size] (

            !:BM = !.Buffer ^ bitmap,
            !:Pos = !.Buffer ^ pos,
            !:Size = !.Buffer ^ size,
            !:State = !.Buffer ^ state,

            % Copy the remaining bits back to the first word of the buffer.
            %
            Remain = !.Size - !.Pos,
            OldPos = !.Pos,
            !:Pos = bits_per_int - Remain,
            ( if Remain > 0 then
                !:BM = !.BM ^ bits(!.Pos,  Remain) :=
                    !.BM ^ bits(OldPos, Remain)
            else
                true
            ),

            % Perform a bulk get from the stream into the buffer
            % starting at the second word.  bit_buffer.read.new
            % guarantees that !.Size is at least as big as bits_per_int.
            %
            ChunkSize = !.Size - bits_per_int,
            StartByteIndex = bits_per_int `unchecked_quotient` bits_per_byte,
            NumBytesToRead = ChunkSize `unchecked_quotient` bits_per_byte,
            Stream = !.Buffer ^ stream,
            stream.bulk_get(Stream, StartByteIndex, NumBytesToRead, !BM,
                NumBytesRead, Result, !State),

            % Record the new size of the buffer if `bulk_get' hit eof
            % or an error.  Further attempts to refill the buffer will
            % do nothing.
            %
            ( if NumBytesRead = NumBytesToRead then
                true
            else
                % XXX We should probably allow the user to attempt to reset
                % the error flag and try again if an error was transient, but
                % the current stream interface doesn't allow for that.
                % If that was allowed we shouldn't modify the size of the
                % buffer or change it to bitmap only here.
                %
                !:Size = NumBytesRead * bits_per_byte + bits_per_int,
                set_use_stream(no, !Buffer)
            ),
            set_all(!.BM, !.Pos, !.Size, !.State, [], !Buffer),
            (
                Result = ok
            ;
                Result = error(_),
                do_set_buffer_error(Result, !Buffer)
            )
        )
    ;
        UseStream = no,
        Result = ok
    ).

finalize(ReadBuffer @ read_buffer(Buffer), Buffer ^ stream, Buffer ^ state,
    Buffer ^ bitmap, Buffer ^ pos, ReadBuffer ^ num_buffered_bits).

    % We didn't have enough bits to satisfy a request, so move the position
    % to the end of the buffer.
    %
:- pred do_set_buffer_error(stream.res(Error)::in,
    bit_buffer(Stream, State, Error)::bit_buffer_di,
    bit_buffer(Stream, State, Error)::bit_buffer_uo) is det.

do_set_buffer_error(Error, !Buffer) :-
    set_read_status(Error, !Buffer).

%---------------------------------------------------------------------------%

% None of these instances work because of limitations in the type and
% RTTI systems.
%
% :- instance stream.stream(read_stream(Stream, Error),
%         read_buffer(Stream, State, Error))
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error)
%     where
% [
%     ( name(_, Name, !Buffer) :-
%         name(!.Buffer ^ read_stream, StreamName,
%             !.Buffer ^ read_buffer_state, State),
%         Name = "bit_buffer.read.read_buffer(" ++ StreamName ++ ")",
%         set_state(State, !Buffer),
%         !:Buffer = unsafe_promise_unique(!.Buffer)
%     )
% ].
%
% :- instance stream.input(read_stream(Stream, Error),
%         read_buffer(Stream, State, Error))
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error)
%     where [].
%
% :- instance stream.reader(read_stream(Stream, Error), bool,
%         read_buffer(Stream, State, Error), Error)
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error)
%     where
% [
%     ( get(_, Result, !Buffer) :-
%         get_bit(Result, !Buffer)
%     )
% ].
%
% :- instance stream.bulk_reader(read_stream(Stream, Error),
%         bit_index, word, read_buffer(Stream, State, Error), Error)
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error)
%     where
% [
%     ( bulk_get(_, Index, NumBits, !Word, NumBitsRead, Result, !Buffer) :-
%         get_bits(Index, NumBits, !Word, NumBitsRead, Result, !Buffer)
%     )
% ].
%
% :- instance stream.bulk_reader(read_stream(Stream, Error),
%         bit_index, bitmap, read_buffer(Stream, State, Error), Error)
%     <= stream.bulk_reader(Stream, bit_index, bitmap, State, Error)
%     where
% [
%     ( bulk_get(_, Index, NumBits, !BM, NumBitsRead, Result, !Buffer) :-
%         get_bitmap(Index, NumBits, !BM, NumBitsRead, Result, !Buffer)
%     )
% ].

%---------------------------------------------------------------------------%
:- end_module bit_buffer.read.
%---------------------------------------------------------------------------%
