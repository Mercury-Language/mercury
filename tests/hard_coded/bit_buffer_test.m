%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%

:- module bit_buffer_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module assoc_list.
:- import_module bit_buffer.
:- import_module bit_buffer.read.
:- import_module bit_buffer.write.
:- import_module bitmap.
:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module io.file.
:- import_module list.
:- import_module pair.
:- import_module stream.
:- import_module string.
:- import_module univ.

:- type request
    --->    bits(word, num_bits)
    ;       bitmap(bitmap, bit_index, num_bits)
    ;       flush
    ;       pad_to_byte
    ;       check_buffer_status(stream.result(univ)).

:- type read_error
        --->    bits(
                    expected_word   :: word,
                    found_word      :: word, num_bits
                )
        ;       bitmap(
                    expected_bitmap :: bitmap,
                    found_bitmap    :: bitmap,
                    request_size    :: num_bits,
                    bits_read       :: num_bits
                )
        ;       check_buffer_status(
                    expected_status :: stream.result(univ),
                    found_status    :: stream.result(univ)
                ).

main(!IO) :-
    % Distinctive byte patterns so we can tell where bits came from.
    %
    Byte1 = 0b10101010,
    Byte2 = 0b11001100,
    Byte3 = 0b01000110,
    Byte4 = 0b10111001,

    % Uncomment this to debug read errors.
    % bit_buffer.read.set_logging_level(1, !IO),

    some [!Seq, !ShortSeq, !LongSeq, !ShortBM, !LongBM, !ErrorSeq] (
        % Test with request sequences that are a minimum of 8 bytes to
        % test overflow even on 64-bit machines (buffers are at least
        % as big as the word size).

        io.write_string("Test reading and writing full bytes.\n", !IO),
        !:Seq = condense(duplicate(4,
            [bits(Byte1, 8), bits(Byte2, 8), check_buffer_status(ok),
            bits(Byte3, 8), bits(0, 0), bits(Byte4, 8)]))
            ++ [check_buffer_status(eof)],
        Seq1 = !.Seq,
        test_sequence(8, !.Seq, !IO),

        io.write_string("Test reading and writing partial bytes.\n", !IO),

        % This also tests a request split over a flush and handling of
        % a list of requests for which the length is not a multiple of
        % the buffer size.

        !:Seq = condense(duplicate(6,
            [bits(Byte1, 7), bits(1, 1), bits(Byte2, 6),
            bits(Byte3, 7), bits(Byte4, 4)])),
        Seq2 = !.Seq,
        test_sequence(8, !.Seq, !IO),

        io.write_string(
    "Test flushes when the stream is at a byte boundary and when it is not.\n",
            !IO),
        !:Seq = condense(duplicate(6,
            [flush, bits(Byte1, 7), bits(0, 1), flush, bits(Byte2, 6),
            bits(Byte3, 7), flush, bits(Byte4, 4)])),
        test_sequence(8, !.Seq, !IO),

        % A short simple bitmap.
        %
        !:Seq = [bits(Byte1, 8), bits(Byte2, 8), bits(Byte3, 8)],
        BM1 = requests_to_bitmap(!.Seq),

        % A longer bitmap.
        %
        BM2 = requests_to_bitmap(Seq1 ++ Seq2),

        io.write_string("Test simple reading and writing of bitmaps.\n", !IO),
        !:Seq = [bitmap(BM1, 0, num_bits(BM1))],
        test_sequence(8, !.Seq, !IO),

        io.write_string("Test a simple offset bitmap read.\n", !IO),
        !:Seq = [bitmap(BM1, bits_per_byte,
                    num_bits(BM1) - bits_per_byte)],
        test_sequence(8, !.Seq, !IO),

        io.write_string("Test zero size requests.\n", !IO),
        !:Seq = [bits(Byte2, 0), bits(Byte1, 4),
                    bits(Byte2, 0), bitmap(BM1, 0, 0)],
        test_sequence(8, !.Seq, !IO),

        io.write_string("Test pad_to_byte\n", !IO),
        !:Seq = [pad_to_byte, bits(Byte1, 3), pad_to_byte, pad_to_byte,
                    bits(Byte2, 8), pad_to_byte, bits(Byte2, 2)],
        test_sequence(8, !.Seq, !IO),

        io.write_string("Test a bitmap that spans multiple buffer flushes.\n",
            !IO),
        !:Seq = [bitmap(BM2, 0, num_bits(BM2))],
        test_sequence(8, !.Seq, !IO),

        io.write_string(
    "Test a bitmap starting at a position that isn't on a byte boundary.\n",
            !IO),
        !:Seq = [bits(Byte1, 3), bitmap(BM2, 0, num_bits(BM2))],
        test_sequence(8, !.Seq, !IO),

        io.write_string("Test offsets passed to put_bitmap.\n", !IO),
        !:Seq = [bits(Byte1, 3), bitmap(BM2, 3, num_bits(BM2) - 3)],
        test_sequence(8, !.Seq, !IO),

        io.write_string("========== Read Error Tests ==========\n", !IO),

        io.write_string("Test unexpected end-of-file.\n", !IO),
        !:ShortSeq = Seq1,
        !:LongSeq = Seq1 ++ Seq1,
        test_error_sequence(io_and_bitmap, 8, !.ShortSeq, !.LongSeq, !IO),

        io.write_string(
            "Test read sequence of bitmaps one byte too long.\n", !IO),
        !:LongBM = shrink_without_copying(copy(BM2), 136),
        !:ShortBM = shrink_without_copying(copy(BM2), 128),
        !:ShortSeq = [bitmap(!.ShortBM, 0, num_bits(!.ShortBM))],
        !:LongSeq = [bitmap(!.LongBM, 0, num_bits(!.LongBM))],
        test_error_sequence(io_and_bitmap, 8, !.ShortSeq, !.LongSeq, !IO),

        io.write_string(
            "Test read sequence of bitmaps one byte too long.\n", !IO),
        !:LongBM = shrink_without_copying(copy(BM2), 136),
        !:ShortBM = shrink_without_copying(copy(BM2), 128),
        !:ShortSeq = [bitmap(!.ShortBM, 0, num_bits(!.ShortBM))],
        !:LongSeq = [bitmap(!.LongBM, 0, num_bits(!.LongBM))],
        test_error_sequence(io_and_bitmap, 8, !.ShortSeq, !.LongSeq, !IO),

        io.write_string("Test non-zero padding bits.\n", !IO),
        PaddingBitsErrorSeq = [bits(Byte1, 2), pad_to_byte],
        test_error_sequence(io_and_bitmap, 8, Seq1, PaddingBitsErrorSeq, !IO),

        % Test cases which only occur with bitmaps of a size not a multiple
        % of bits_per_byte.
        %
        io.write_string("========== Bitmap error tests ==========\n", !IO),
        io.write_string("Test eof when skipping padding in bitmap\n", !IO),
        !:Seq = [bits(0, 7)],
        !:ErrorSeq = [bits(0, 1), pad_to_byte],
        test_error_sequence(bitmap_only, 8, !.Seq, !.ErrorSeq, !IO),

        io.write_string("========== Argument Error Tests ==========\n", !IO),
        test_error_sequence(io_and_bitmap, 8, Seq1, [bits(0, -1)], !IO),
        test_error_sequence(io_and_bitmap, 8, Seq1, [bits(0, 100)], !IO),
        test_error_sequence(io_and_bitmap, 8, Seq1, [bitmap(BM1, 0, -1)], !IO),
        test_error_sequence(io_and_bitmap, 8, Seq1,
            [bitmap(BM1, 0, 10000)], !IO),

        io.write_string("========== Stream Error Tests ==========\n", !IO),
        test_error_sequence(timebomb(10), 8, Seq1, Seq1, !IO),

        io.write_string("Test error when refilling buffer\n", !IO),
        !:Seq = [bitmap(shrink_without_copying(copy(BM2), 72), 0, 72)],
        !:ErrorSeq = [bits(BM2 ^ bits(0, 32), 32),
                        bits(BM2 ^ bits(32, 32), 32),
                        check_buffer_status(ok),
                        bits(BM2 ^ bits(64, 8), 8)],
        test_error_sequence(timebomb(8), 8, !.Seq, !.ErrorSeq, !IO)
    ),
    io.file.remove_file(bit_buffer_test_tmp_file, _, !IO).

:- pred test_sequence(num_bytes::in, list(request)::in, io::di, io::uo) is det.

test_sequence(BufferSize, Requests0, !IO) :-
    % This makes the results for bitmap and I/O buffers consistent.
    Requests = Requests0 ++ [pad_to_byte],

    io.format("Testing with buffer size %d.\n", [i(BufferSize)], !IO),
    TempFile = bit_buffer_test_tmp_file,
    io.write_string("Testing writes: [", !IO),
    io.write_list(Requests, ", ", output_request, !IO),
    io.write_string("]\n", !IO),
    io.write_string("Expected result: ", !IO),
    ExpectedBM = requests_to_bitmap(Requests),
    io.write_string(to_byte_string(ExpectedBM), !IO),
    io.nl(!IO),
    io.flush_output(!IO),
    test_writes(BufferSize, TempFile, Requests, ExpectedBM, !IO),
    io.write_string("Testing reads:\n", !IO),
    test_reads(BufferSize, TempFile, Requests, ExpectedBM, !IO),
    io.write_string("\n", !IO),
    io.flush_output(!IO).

    % Read the given number of bits, then fail on the next.
:- type timer == int.

:- type error_test_type
    --->    io_and_bitmap
    ;       bitmap_only
    ;       timebomb(timer).

    % SetupRequests will set up a bitmap and the file returned by
    % `bit_buffer_test_tmp_file'.  Requests is a list of requests
    % that will result in a read error when applied to that input.
    %
:- pred test_error_sequence(error_test_type::in, num_bytes::in,
    list(request)::in, list(request)::in, io::di, io::uo) is cc_multi.

test_error_sequence(ErrorTestType, BufferSize,
        SetupRequests0, Requests0, !IO) :-
    (
        ErrorTestType = io_and_bitmap,
        % This makes the results for bitmap and I/O buffers consistent.
        Requests = Requests0 ++ [pad_to_byte],
        SetupRequests = SetupRequests0 ++ [pad_to_byte]
    ;
        ErrorTestType = bitmap_only,
        SetupRequests = SetupRequests0,
        Requests = Requests0
    ;
        ErrorTestType = timebomb(_),
        SetupRequests = SetupRequests0,
        Requests = Requests0
    ),

    io.write_string("Testing sequence that should cause an error:\n", !IO),
    io.write_string("Using setup requests:\n", !IO),
    io.write(SetupRequests, !IO),
    io.nl(!IO),
    io.write_string("Using error requests:\n", !IO),
    io.write(Requests, !IO),
    io.nl(!IO),
    ExpectedBM = requests_to_bitmap(SetupRequests),
    TempFile = bit_buffer_test_tmp_file,
    ( if
        ( ErrorTestType = io_and_bitmap
        ; ErrorTestType = timebomb(_)
        )
    then
        test_writes(8, TempFile, SetupRequests, ExpectedBM, !IO)
    else
        true
    ),
    ( if
        ( ErrorTestType = io_and_bitmap
        ; ErrorTestType = bitmap_only
        )
    then
        check_that_error_occurs("bitmap",
            test_bitmap_reads(Requests, ExpectedBM),
            !IO)
    else
        true
    ),
    ( if
        ErrorTestType = io_and_bitmap
    then
        check_that_error_occurs("I/O",
            test_io_reads(BufferSize, TempFile, Requests),
            !IO)
    else
        true
    ),
    ( if
        ErrorTestType = timebomb(Timer)
    then
        check_that_error_occurs("stream read error",
            test_io_timebomb_reads(BufferSize, Timer, TempFile, Requests),
            !IO)
    else
        true
    ),

    io.write_string("\n", !IO),
    io.flush_output(!IO).

:- pred check_that_error_occurs(string::in,
    pred(io, io)::(pred(di, uo) is det), io::di, io::uo) is cc_multi.

check_that_error_occurs(Desc, P, !IO) :-
    Q = (pred({}::out, !.IO::di, !:IO::uo) is det :- P(!IO)),
    try_io(Q, Result, !IO),
    (
        Result = succeeded(_),
        io.write_string(Desc ++ " reads unexpectedly succeeded\n", !IO)
    ;
        Result = exception(Error),
        io.write_string(Desc ++ " reads failed as expected:\n", !IO),
        io.write_line(Error, !IO)
    ).

:- pred output_request(request::in, io::di, io::uo) is det.

output_request(bits(Word, NumBits), !IO) :-
    io.write_string("bits(", !IO),
    io.write_string(int_to_base_string(Word, 2), !IO),
    io.write_string(", ", !IO),
    io.write_int(NumBits, !IO),
    io.write_string(")", !IO).
output_request(bitmap(BM, Index, NumBits), !IO) :-
    io.write_string("bitmap(", !IO),
    io.write_string(bitmap.to_byte_string(BM), !IO),
    io.write_string(", ", !IO),
    io.write_int(Index, !IO),
    io.write_string(", ", !IO),
    io.write_int(NumBits, !IO),
    io.write_string(")", !IO).
output_request(pad_to_byte, !IO) :-
    io.write_string("pad_to_byte", !IO).
output_request(flush, !IO) :-
    io.write_string("flush", !IO).
output_request(check_buffer_status(BufferStatus), !IO) :-
    io.write_string("check_buffer_status(", !IO),
    io.write(BufferStatus, !IO),
    io.write_string(")", !IO).

:- pred test_writes(num_bytes::in, string::in, list(request)::in,
    bitmap::in, io::di, io::uo) is det.

test_writes(BufferSize, FileName, Writes, ExpectedBM, !IO) :-
    io.open_binary_output(FileName, WriteOpenRes, !IO),
    (
        WriteOpenRes = ok(WriteStream),
        some [!BMBuffer, !IOBuffer] (
            !:BMBuffer = new_bitmap_builder(BufferSize),
            !:IOBuffer = new(BufferSize, WriteStream, !.IO),

            list.foldl(do_write, Writes, !BMBuffer),
            list.foldl(do_write, Writes, !IOBuffer),

            finalize(!.IOBuffer, _, !:IO),
            BM = finalize_to_bitmap(!.BMBuffer),
            io.close_binary_output(WriteStream, !IO)
        ),

        ( if BM = ExpectedBM then
            io.write_string("Collected bitmap compares OK.\n", !IO)
        else
            io.write_string("Collected bitmap differs: \n", !IO),
            io.write_string(to_byte_string(BM), !IO),
            io.nl(!IO),
            io.flush_output(!IO)
        ),

        io.open_binary_input(FileName, ReadOpenRes, !IO),
        (
            ReadOpenRes = ok(ReadStream),
            io.read_binary_file_as_bitmap(ReadStream, BMReadResult, !IO),
            (
                BMReadResult = ok(ReadBM),
                ( if ReadBM = ExpectedBM then
                    io.write_string("I/O bitmap compares OK.\n", !IO),
                    io.flush_output(!IO)
                else
                    io.write_string("I/O bitmap differs: \n", !IO),
                    io.write_string(to_byte_string(ReadBM), !IO),
                    io.nl(!IO),
                    io.flush_output(!IO)
                )
            ;
                BMReadResult = error(Error),
                io.write_string(io.error_message(Error), !IO),
                io.nl(!IO),
                io.flush_output(!IO)
            ),
            io.close_binary_input(ReadStream, !IO)
        ;
            ReadOpenRes = error(Msg),
            io.write_string(io.error_message(Msg), !IO),
            io.nl(!IO),
            io.flush_output(!IO)
        )
    ;
        WriteOpenRes = error(Msg),
        io.write_string(io.error_message(Msg), !IO),
        io.nl(!IO),
        io.flush_output(!IO)
    ).

:- pred do_write(request::in,
    write_buffer(S, St)::write_buffer_di,
    write_buffer(S, St)::write_buffer_uo) is det
    <= stream.writer(S, bitmap.slice, St).

do_write(bits(Word, NumBits), !Buffer) :-
    ( if NumBits = 1 then
        Bit = ( if Word = 0 then no else yes ),
        put_bit(Bit, !Buffer)
    else if NumBits = 8 then
        put_byte(Word, !Buffer)
    else
        put_bits(Word, NumBits, !Buffer)
    ).
do_write(bitmap(BM, Index, NumBits), !Buffer) :-
    ( if Index = 0, NumBits = BM ^ num_bits then
        put_bitmap(BM, !Buffer)
    else
        put_bitmap(BM, Index, NumBits, !Buffer)
    ).
do_write(pad_to_byte, !Buffer) :-
    NumPaddingBits = num_bits_to_byte_boundary(!.Buffer),
    put_bits(0, NumPaddingBits, !Buffer).
do_write(flush, !Buffer) :-
    flush(!Buffer).
do_write(check_buffer_status(_), !Buffer).

    % Create a bitmap directly from the list of requests.
:- func requests_to_bitmap(list(request)::in) =
    (bitmap::bitmap_uo) is det.

requests_to_bitmap(Requests) = !:BM :-
    Size = request_list_length(Requests, 0),
    !:BM = bitmap.init(Size),
    list.foldl2(request_to_bitmap, Requests, 0, _, !BM).

:- func request_list_length(list(request), int) = int.

request_list_length([], L) = L.
request_list_length([Req | Reqs], L0) = L :-
    ( Req = bits(_, NumBits)
    ; Req = bitmap(_, _, NumBits)
    ; Req = pad_to_byte, Rem = L0 `rem` bits_per_byte,
        NumBits = ( if Rem = 0 then 0 else bits_per_byte - Rem )
    ; Req = flush, NumBits = 0
    ; Req = check_buffer_status(_), NumBits = 0
    ),
    L = request_list_length(Reqs, L0 + NumBits).

:- pred request_to_bitmap(request::in, int::in, int::out,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

request_to_bitmap(bits(Word, NumBits), !Index, !BM) :-
    !:BM = !.BM ^ bits(!.Index, NumBits) := Word,
    !:Index = !.Index + NumBits.
request_to_bitmap(bitmap(OtherBM, Start, NumBits), !Index, !BM) :-
    !:BM = copy_bits(OtherBM, Start, !.BM, !.Index, NumBits),
    !:Index = !.Index + NumBits.
request_to_bitmap(pad_to_byte, !Index, !BM) :-
    Rem = !.Index `rem` bits_per_byte,
    NumBits = ( if Rem = 0 then 0 else bits_per_byte - Rem ),
    !:BM = !.BM ^ bits(!.Index, NumBits) := 0,
    !:Index = !.Index + NumBits.
request_to_bitmap(flush, !Index, !BM).
request_to_bitmap(check_buffer_status(_), !Index, !BM).

:- pred test_reads(num_bytes::in, string::in, list(request)::in,
    bitmap::in, io::di, io::uo) is det.

test_reads(BufferSize, FileName, Requests, ExpectedBM, !IO) :-
    test_bitmap_reads(Requests, ExpectedBM, !IO),
    test_io_reads(BufferSize, FileName, Requests, !IO).

:- pred test_bitmap_reads(list(request)::in,
    bitmap::in, io::di, io::uo) is det.

test_bitmap_reads(Requests, ExpectedBM, !IO) :-
    some [!BMBuffer] (
        !:BMBuffer = new_bitmap_reader(ExpectedBM, 0, ExpectedBM ^ num_bits),
        do_reads("bitmap", 1, Requests, !BMBuffer),
        finalize(!.BMBuffer, _, _, _, _, BMNumFinalBits),
        ( if BMNumFinalBits = 0 then
            true
        else
            throw(string.format("bitmap reader has %d bits left over: \n",
                [i(BMNumFinalBits)]) : string)
        ),
        io.write_string("bitmap read tests completed.\n", !IO)
    ).

:- pred test_io_reads(num_bytes::in, string::in, list(request)::in,
    io::di, io::uo) is det.

test_io_reads(BufferSize, FileName, Requests, !IO) :-
    io.open_binary_input(FileName, ReadOpenRes, !IO),
    some [!IOBuffer] (
        ReadOpenRes = ok(ReadStream),
        !:IOBuffer = new(BufferSize, ReadStream, !.IO) : io_read_buffer,
        do_reads("I/O", 1, Requests, !IOBuffer),
        finalize(!.IOBuffer, _, !:IO, _, _, IONumFinalBits),
        ( if IONumFinalBits = 0 then
            true
        else
            throw(string.format("I/O reader has %d bits left over: \n",
                [i(IONumFinalBits)]): string)
        ),
        io.write_string("I/O read tests completed.\n", !IO),
        io.close_binary_input(ReadStream, !IO)
    ;
        ReadOpenRes = error(Msg),
        throw(Msg)
    ).

:- pred test_io_timebomb_reads(num_bytes::in, num_bytes::in,
    string::in, list(request)::in, io::di, io::uo) is det.

test_io_timebomb_reads(BufferSize, Countdown, FileName, Requests, !IO) :-
    io.open_binary_input(FileName, ReadOpenRes, !IO),
    some [!ErrorBuffer] (
        ReadOpenRes = ok(ReadStream),
        ErrorState0 = 'new timebomb_state'(ReadStream, !.IO, Countdown),
        !:ErrorBuffer = bit_buffer.read.new(BufferSize,
            timebomb_byte_stream,
            unsafe_promise_unique(ErrorState0)) :
            read_buffer(timebomb_byte_stream,
                timebomb_state, timebomb_error),

        do_reads("timebomb", 1, Requests, !ErrorBuffer),
        finalize(!.ErrorBuffer, _, ErrorState, _, _, ErrorNumFinalBits),
        ( if ErrorNumFinalBits = 0 then
            true
        else
            throw(string.format("timebomb reader has %d bits left over: \n",
                [i(ErrorNumFinalBits)]) : string)
        ),
        det_univ_to_type(univ(ErrorState ^ timebombed_state), !:IO),
        !:IO = unsafe_promise_unique(!.IO),
        io.write_string("timebomb read tests completed.\n", !IO),
        io.close_binary_input(ReadStream, !IO)
    ;
        ReadOpenRes = error(Msg),
        throw(Msg)
    ).

:- pred do_reads(string::in, int::in, list(request)::in,
    read_buffer(S, St, E)::read_buffer_di,
    read_buffer(S, St, E)::read_buffer_uo) is det
    <= stream.bulk_reader(S, byte_index, bitmap, St, E).

do_reads(_, _, [], !Buffer).
do_reads(Desc, Index, [Req | Reqs], !Buffer) :-
    do_read(Desc, Index, Req, !Buffer),
    do_reads(Desc, Index + 1, Reqs, !Buffer).

:- pred do_read(string::in, int::in, request::in,
    read_buffer(S, St, E)::read_buffer_di,
    read_buffer(S, St, E)::read_buffer_uo) is det
    <= stream.bulk_reader(S, byte_index, bitmap, St, E).

do_read(Desc, ReqIndex, bits(ExpectedWord0, NumBits), !Buffer) :-
    ExpectedWord = mask_word(ExpectedWord0, NumBits),
    ( if NumBits = 1 then
        get_bit(GetResult, !Buffer),
        (
            GetResult = ok(ResultBit),
            ResultWord = ( if ResultBit = yes then 1 else 0 ),
            ( if ResultWord = ExpectedWord then
                true
            else
                throw_read_result_error(bits(ExpectedWord, ResultWord, 1),
                    Desc, ReqIndex)
            )
        ;
            GetResult = eof,
            throw("bit_buffer_test: unexpected eof in get_bit")
        ;
            GetResult = error(Err),
            throw(Err)
        )
    else
        get_bits(bits_per_int - NumBits, NumBits, 0, ResultWord,
            NumBitsRead, GetResult, !Buffer),
        (
            GetResult = ok,
            ( if NumBitsRead = NumBits, ExpectedWord = ResultWord then
                true
            else
                throw_read_result_error(
                    bits(ExpectedWord, ResultWord, NumBits),
                    Desc, ReqIndex)
            )
        ;
            GetResult = error(Err),
            throw(Err)
        )
    ).

do_read(Desc, ReqIndex, bitmap(SourceBM, Index, NumBits), !Buffer) :-
    some [!BM] (
        !:BM = bitmap.init(SourceBM ^ num_bits),
        ( if Index = 0, NumBits = SourceBM ^ num_bits then
            get_bitmap(!BM, BitsRead, GetResult, !Buffer)
        else
            get_bitmap(Index, NumBits, !BM, BitsRead, GetResult, !Buffer)
        ),
        (
            GetResult = ok,
            ExpectedBM0 = bitmap.init(SourceBM ^ num_bits),
            ExpectedBM = copy_bits(SourceBM, Index,
                            ExpectedBM0, Index, NumBits),
            ( if ExpectedBM = !.BM then
                true
            else
                throw_read_result_error(
                    bitmap(ExpectedBM, !.BM, NumBits, BitsRead),
                    Desc, ReqIndex)
            )
        ;
            GetResult = error(Err),
            throw_read_stream_error(Err, Desc, ReqIndex)
        )
    ).
do_read(Desc, ReqIndex, pad_to_byte, !Buffer) :-
    NumPaddingBits = num_bits_to_byte_boundary(!.Buffer),
    do_read(Desc, ReqIndex, bits(0, NumPaddingBits), !Buffer).
do_read(_Desc, _Index, flush, !Buffer).
do_read(Desc, ReqIndex, check_buffer_status(ExpectedStatus), !Buffer) :-
    buffer_status(FoundStatus0, !Buffer),
    ( FoundStatus0 = ok, FoundStatus = ok
    ; FoundStatus0 = eof, FoundStatus = eof
    ; FoundStatus0 = error(Err), FoundStatus = error(univ(Err))
    ),
    ( if ExpectedStatus = FoundStatus then
        true
    else
        throw_read_result_error(
            check_buffer_status(ExpectedStatus, FoundStatus),
            Desc, ReqIndex)
    ).

:- type read_result_exception == pair(string, read_error).

:- pred throw_read_result_error(read_error::in, string::in, int::in)
    is erroneous.

throw_read_result_error(Error, Desc, ReqIndex) :-
    throw((Desc ++ ": error in request " ++ int_to_string(ReqIndex)) - Error).

:- pred throw_read_stream_error(T::in, string::in, int::in)
    is erroneous.

throw_read_stream_error(Error, Desc, ReqIndex) :-
    throw((Desc ++ ": error in request " ++ int_to_string(ReqIndex)) - Error).

:- func mask_word(word, num_bits) = word.

mask_word(ExpectedWord0, N) = ExpectedWord :-
    ( if N \= 0 then
        BitMask  = 1 `unchecked_left_shift` (N - 1),
        BitsMask = BitMask \/ (BitMask - 1),
        ExpectedWord = ExpectedWord0 /\ BitsMask
    else
        ExpectedWord = 0
    ).

:- func bit_buffer_test_tmp_file = string.

bit_buffer_test_tmp_file = "bit_buffer_test_tmp".

    % A timebomb stream counts down bytes until it returns an error.
    % XXX It should be possible to produce a generic version of this
    % that works on all Unit types, but the current restrictions on
    % instances don't allow that.  Also, the Error type of the stream
    % can't be exposed for the same reason.
    %
:- type timebomb_byte_stream
    --->    timebomb_byte_stream.
:- type timebomb_state
    --->    some [Stream, State, Error]
            timebomb_state(
                timebombed_stream   :: Stream,
                timebombed_state    :: State,
                countdown           :: int
            ) =>
                (reader(Stream, byte, State, Error),
                bulk_reader(Stream, int, bitmap, State, Error)).

:- type timebomb_error
        --->    bang
        ;       already_exploded
        ;       stream_error(univ).

:- instance stream.error(timebomb_error) where [
    error_message(bang) = "Bang!!!",
    error_message(already_exploded) = "This stream is already exploded.",
    error_message(stream_error(_Univ)) = "stream_error"
].

:- instance stream.stream(timebomb_byte_stream, timebomb_state) where [
    name(_, "timebomb_byte_stream", !State)
].

:- instance stream.input(timebomb_byte_stream, timebomb_state) where [].

:- instance stream.reader(timebomb_byte_stream, byte,
    timebomb_state, timebomb_error)
where [
    (get(_Stream, Result, !.State, unsafe_promise_unique(!:State)) :-
        !.State = timebomb_state(TStream, TState0, Countdown0),
        ( if Countdown0 < 0 then
            Result = error(already_exploded)
        else if Countdown0 = 0 then
            Result = error(bang),
            !:State = 'new timebomb_state'(TStream, TState0, -1)
        else
            get(TStream, ByteResult, unsafe_promise_unique(TState0), TState),
            (
                ByteResult = ok(Byte),
                Countdown = Countdown0 - 1,
                Result = ok(Byte)
            ;
                ByteResult = eof,
                Countdown = -1,
                Result = eof
            ;
                ByteResult = error(Error),
                Countdown = -1,
                Result = error(stream_error(univ(Error)))
            ),
            !:State = 'new timebomb_state'(TStream, TState, Countdown)
        )
    )
].

:- instance stream.bulk_reader(timebomb_byte_stream, int, bitmap,
    timebomb_state, timebomb_error)
where [
    (bulk_get(_, Index, NumBytes, !BM, NumBytesRead, Result,
            !.State, unsafe_promise_unique(!:State)) :-
        !.State = timebomb_state(TStream, TState0, Countdown0),
        ( if Countdown0 < 0 then
            NumBytesRead = 0,
            Result = error(already_exploded)
        else if Countdown0 = 0 then
            NumBytesRead = 0,
            Result = error(bang),
            !:State = 'new timebomb_state'(TStream, TState0, -1)
        else
            unsafe_promise_unique(TState0, TState1),
            bulk_get(TStream, Index, NumBytes, !BM, NumBytesRead0,
                BulkGetResult, TState1, TState),
            (
                BulkGetResult = ok,
                ( if NumBytesRead0 >= Countdown0 then
                    NumBytesRead = Countdown0,
                    Result = error(bang)
                else
                    NumBytesRead = NumBytesRead0,
                    Result = ok
                ),
                Countdown = Countdown0 - NumBytesRead
            ;
                BulkGetResult = error(Error),
                NumBytesRead = 0,
                Countdown = -1,
                Result = error(stream_error(univ(Error)))
            ),
            !:State = 'new timebomb_state'(TStream, TState, Countdown)
        )
    )
].
