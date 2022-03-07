%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%

:- module bitmap_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bitmap.
:- import_module bitmap_tester.
:- import_module bitmap_simple.
:- import_module bool.
:- import_module deconstruct.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module univ.

main(!IO) :-
    try_io(run_test, Res, !IO),
    (
        Res = succeeded(_)
    ;
        Res = exception(Excp),
        io.set_exit_status(1, !IO),
        ( if univ_to_type(Excp, BitmapResErr) then
            io.nl(!IO),
            write_bitmap_result_error(BitmapResErr, !IO)
        else
            rethrow(Res)
        )
    ).

:- pred run_test({}::out, io::di, io::uo) is cc_multi.

run_test({}, !IO) :-
    some [!BM] (
        io.write_string("Single byte bitmap\n", !IO),
        !:BM = bitmap_tester.new(4, yes),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        write(!.BM ^ bit(0), !IO),
        nl(!IO),
        !:BM = !.BM ^ bit(1) := no,
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        write(!.BM ^ bit(1), !IO),
        nl(!IO),
        !:BM = !.BM ^ bit(2) := no,
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        write_binary_string(!.BM ^ bits(0, 4), !IO),
        nl(!IO),
        !:BM = !.BM ^ bits(0, 2) := \ (!.BM ^ bits(0, 2)),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = !.BM ^ bits(0, 0) := !.BM ^ bits(4, 0),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO)
    ),
    some [!BM] (
        io.write_string("Multi-byte bitmap\n", !IO),
        !:BM = bitmap_tester.new(20, no),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = flip(!.BM, 1),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = ((((((!.BM  ^ bit(3) := yes)
            ^ bit(6) := yes)
            ^ bit(8) := yes)
            ^ bit(10) := yes)
            ^ bit(12) := yes)
            ^ bit(17) := yes),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        write_binary_string(!.BM ^ bits(1, 10), !IO),
        nl(!IO),
        write_binary_string(!.BM ^ bits(8, 12), !IO),
        nl(!IO),
        !:BM = !.BM ^ bits(6, 12) := \ (!.BM ^ bits(6, 12)),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = flip(!.BM, 6),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = complement(!.BM),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = resize(!.BM, 32, yes),
        io.write_string(to_string(!.BM ^ fst), !IO),
        nl(!IO),
        functor(!.BM ^ fst, do_not_allow, Functor, _),
        io.write_string(Functor, !IO),
        nl(!IO)
    ),
    some [!BM] (
        io.write_string("Longer bitmap\n", !IO),
        !:BM = bitmap_tester.new(160, no),
        BytePattern = 0b10111001,
        fill_in_alternating_pattern(BytePattern, !BM),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        !:BM = !.BM ^ bits(6, 12) := \ (!.BM ^ bits(6, 12)),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),
        io.write_string("non-overlapping copy_bits\n", !IO),
        !:BM = copy_bits_in_bitmap(!.BM, 12, 64, 32),
        io.write_string(to_byte_string(!.BM), !IO),
        nl(!IO),

        io.write_string("testing builtin.copy\n", !IO),
        builtin.copy(!.BM ^ fst, CopyBM),
        io.write_string(to_byte_string(CopyBM), !IO),
        nl(!IO),
        ( if CopyBM = !.BM ^ fst then
            io.write_string("Copy succeeded\n", !IO)
        else
            io.write_string("Copy failed\n", !IO)
        )
    ),

    io.write_string("Test simple aligned byte block copy.\n", !IO),
    test_copy(8, 64, 32, !IO),

    io.write_string("Test byte block copy with extra bits on ends.\n", !IO),
    test_copy(6, 62, 36, !IO),

    io.write_string("Test unaligned copy.\n", !IO),
    test_copy(7, 64, 32, !IO),

    io.write_string("Test unaligned copy with extra bits on ends.\n", !IO),
    test_copy(7, 67, 36, !IO),

    io.write_string("Test overlapping aligned byte block copy.\n", !IO),
    test_copy(8, 0, 36, !IO),

    io.write_string("Test overlapping aligned byte block copy.\n", !IO),
    test_copy(0, 8, 36, !IO),

    io.write_string("Test overlapping unaligned copy.\n", !IO),
    test_copy(2, 1, 36, !IO),

    io.write_string("Test overlapping unaligned copy.\n", !IO),
    test_copy(1, 2, 36, !IO),

    io.write_string("Test copy to same position.\n", !IO),
    test_copy(1, 1, 36, !IO),

    io.write_string("Test copy to end of bitmap.\n", !IO),
    test_copy(0, 1, 166, !IO),

    io.write_string("Test zero bit copies.\n", !IO),
    test_copy(0, 1, 0, !IO),
    test_copy(0, 167, 0, !IO),

    test_set_op("union", union, !IO),
    test_set_op("intersect", intersect, !IO),
    test_set_op("difference", difference, !IO),
    test_set_op("xor", xor, !IO),

    test_binary_op("ordering", bitmap_tester.ordering, !IO),
    test_binary_op("test_unify", bitmap_tester.test_unify, !IO),

    test_text_io(!IO),
    test_binary_io(!IO),

    some [!BM] (
        !:BM = bitmap.init(64, yes),
        !:BM = !.BM ^ bits(32, 16) := 0b1011011100100101,
        test_exception(
            ((pred) is semidet :-
                _ = !.BM ^ bit(-1)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = !.BM ^ bit(64)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = !.BM ^ bit(73)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy_bits_in_bitmap(copy(!.BM), -1, 1, 32)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy_bits_in_bitmap(copy(!.BM), 33, 32, 32)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy_bits_in_bitmap(copy(!.BM), 32, 33, 32)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy(!.BM) ^ bits(-1, 32)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy(!.BM) ^ bits(33, 32)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy(!.BM) ^ bits(0, 65)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy(!.BM) ^ bits(0, -1)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy(!.BM) ^ bits(65, 0)
            ), !IO),
        test_exception(
            ((pred) is semidet :-
                _ = copy(!.BM) ^ bits(-1, 0)
            ), !IO)
    ).

:- pred test_exception((pred)::in((pred) is semidet),
    io::di, io::uo) is cc_multi.

test_exception(Pred, !IO) :-
    try((pred({}::out) is semidet :- Pred), Result),
    (
        Result = succeeded(_),
        io.write_string("Error: test succeeded, expected exception\n", !IO)
    ;
        Result = failed,
        io.write_string("Error: test failed, expected exception\n", !IO)
    ;
        Result = exception(Exception),
        io.write_string("Found exception as expected: ", !IO),
        io.write(univ_value(Exception), !IO),
        io.nl(!IO)
    ).

    % Do the copy tests to a few different bitmaps, to make sure
    % correct results aren't a fluke of the original contents, and
    % to check that the copy isn't disturbing bits outside the
    % destination range.
    %
:- pred test_copy(bit_index::in, bit_index::in, num_bits::in,
    io::di, io::uo) is det.

test_copy(SrcStart, DestStart, NumBits, !IO) :-
    BMLength = 167,
    some [!DestBM, !SrcBM] (
        io.format("Copy %i %i %i\n", [i(SrcStart), i(DestStart), i(NumBits)],
            !IO),

        !:SrcBM = bitmap_tester.new(BMLength, no),
        BytePattern = 0b10111001,
        fill_in_alternating_pattern(BytePattern, !SrcBM),

        io.write_string("Copy to zeroed bitmap\n", !IO),
        !:DestBM = bitmap_tester.new(BMLength, no),
        !:DestBM = copy_bits(!.SrcBM, SrcStart, !.DestBM, DestStart, NumBits),
        io.write_string(to_byte_string(!.DestBM), !IO),
        nl(!IO),

        io.write_string("Copy to filled bitmap\n", !IO),
        !:DestBM = bitmap_tester.new(BMLength, yes),
        !:DestBM = copy_bits(!.SrcBM, SrcStart, !.DestBM, DestStart, NumBits),
        io.write_string(to_byte_string(!.DestBM), !IO),
        nl(!IO),

        io.write_string("Copy to alternating bitmap\n", !IO),
        !:DestBM = bitmap_tester.new(BMLength, yes),
        fill_in_alternating_pattern(0b10101010, !DestBM),
        !:DestBM = copy_bits(!.SrcBM, SrcStart, !.DestBM, DestStart, NumBits),
        io.write_string(to_byte_string(!.DestBM), !IO),
        nl(!IO),

        io.write_string("Copy to same bitmap\n", !IO),
        !:DestBM = copy_bits_in_bitmap(!.SrcBM, SrcStart, DestStart, NumBits),
        io.write_string(to_byte_string(!.DestBM), !IO),
        nl(!IO)
    ).

:- pred test_set_op(string, (func(tbitmap, tbitmap) = tbitmap), io, io).
:- mode test_set_op(in, (func(tbitmap_ui, tbitmap_di) = tbitmap_uo is det),
    di, uo) is det.

test_set_op(OpStr, Op, !IO) :-
    test_binary_op(OpStr, Op,
            (pred(TBM::in, !.IO::di, !:IO::uo) is det :-
                io.write_string(to_byte_string(TBM ^ fst), !IO)
            ), !IO).

:- pred test_binary_op(string, (func(tbitmap, tbitmap) = T), io, io).
:- mode test_binary_op(in, (func(tbitmap_ui, tbitmap_di) = tbitmap_uo is det),
    di, uo) is det.
:- mode test_binary_op(in, (func(tbitmap_ui, tbitmap_di) = out is det),
    di, uo) is det.

test_binary_op(OpStr, Op, !IO) :-
    test_binary_op(OpStr, Op, io.write, !IO).

:- pred test_binary_op(string, (func(tbitmap, tbitmap) = T),
    pred(T, io, io), io, io).
:- mode test_binary_op(in, (func(tbitmap_ui, tbitmap_di) = tbitmap_uo is det),
    (pred(in, di, uo) is det), di, uo) is det.
:- mode test_binary_op(in, (func(tbitmap_ui, tbitmap_di) = out is det),
    (pred(in, di, uo) is det), di, uo) is det.

test_binary_op(OpStr, Op, Writer, !IO) :-
    test_binary_op(8, OpStr, Op, Writer, !IO),
    test_binary_op(64, OpStr, Op, Writer, !IO).

:- pred test_binary_op(int, string, (func(tbitmap, tbitmap) = T),
    pred(T, io, io), io, io).
:- mode test_binary_op(in, in,
    (func(tbitmap_ui, tbitmap_di) = tbitmap_uo is det),
    (pred(in, di, uo) is det), di, uo) is det.
:- mode test_binary_op(in, in, (func(tbitmap_ui, tbitmap_di) = out is det),
    (pred(in, di, uo) is det), di, uo) is det.

test_binary_op(BMLength, OpStr, Op, Writer, !IO) :-
    ZeroedBM = bitmap_tester.new(BMLength, no),
    OnesBM = bitmap_tester.new(BMLength, yes),

    PatternBM0 = bitmap_tester.new(BMLength, no),
    BytePattern = 0b10111001,
    fill_in_alternating_pattern(BytePattern, PatternBM0, PatternBM),

    AlternatingBM0 = bitmap_tester.new(BMLength, yes),
    fill_in_alternating_pattern(0b10101010, AlternatingBM0, AlternatingBM),

    test_binary_op_2("zeroes", ZeroedBM, OpStr, Op,
        "pattern", PatternBM, Writer, !IO),
    test_binary_op_2("ones", OnesBM, OpStr, Op,
        "pattern", PatternBM, Writer, !IO),
    test_binary_op_2("pattern", PatternBM, OpStr, Op,
        "ones", OnesBM, Writer, !IO),
    test_binary_op_2("pattern", PatternBM, OpStr, Op,
        "zeroes", ZeroedBM, Writer, !IO),
    test_binary_op_2("pattern", PatternBM, OpStr, Op,
        "alternating", AlternatingBM, Writer, !IO),
    test_binary_op_2("pattern", PatternBM, OpStr, Op,
        "pattern", PatternBM, Writer, !IO),
    test_binary_op_2("alternating", AlternatingBM, OpStr, Op,
        "alternating", AlternatingBM, Writer, !IO).

:- pred test_binary_op_2(string, tbitmap, string, (func(tbitmap, tbitmap) = T),
    string, tbitmap, pred(T, io, io), io, io).
:- mode test_binary_op_2(in, tbitmap_ui,
    in, (func(tbitmap_ui, tbitmap_di) = tbitmap_uo is det),
    in, tbitmap_ui, (pred(in, di, uo) is det), di, uo) is det.
:- mode test_binary_op_2(in, tbitmap_ui,
    in, (func(in, in) = out is det),
    in, tbitmap_ui, (pred(in, di, uo) is det), di, uo) is det.

test_binary_op_2(BMStr1, BM1, OpStr, Op, BMStr2, BM2, Writer, !IO) :-
    io.write_string(OpStr, !IO),
    io.write_string("(", !IO),
    io.write_string(BMStr1, !IO),
    io.write_string(", ", !IO),
    io.write_string(BMStr2, !IO),
    io.write_string(") = ", !IO),
    Writer(BM1 `Op` copy(BM2), !IO),
    io.nl(!IO).

:- pred test_binary_io(io::di, io::uo) is det.

test_binary_io(!IO) :-
    FileName = "bitmap_test_output",
    BMa0 = bitmap.init(64, yes),
    BMa = BMa0 ^ bits(32, 16) := 0b1011011100100101,
    BMb0 = bitmap.init(47, yes),
    BMb = BMb0 ^ bits(11, 16) := 0b1011010110100101,
    io.open_binary_output(FileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        bitmap.write_bitmap(Stream, BMa, !IO),
        bitmap.write_bitmap_range(Stream, BMb, 2, 3, !IO),
        io.close_binary_output(Stream, !IO),
        io.open_binary_input(FileName, OpenInputRes, !IO),
        (
            OpenInputRes = ok(IStream),
            InputBMa0 = bitmap.init(64, no),
            bitmap.read_bitmap(IStream, InputBMa0, InputBMa,
                BytesReadA, ReadResA, !IO),
            ( if
                ReadResA = ok,
                BytesReadA = 8,
                InputBMa = BMa
            then
                io.write_string("First read succeeded\n", !IO)
            else
                io.write_string("First read failed\n", !IO)
            ),
            InputBMb0 = bitmap.init(32, no),
            bitmap.read_bitmap(IStream, InputBMb0, InputBMb1,
                BytesReadB, ReadResB, !IO),

            % Check that we are at eof.
            bitmap.read_bitmap(IStream, InputBMb1, InputBMb,
                BytesReadB2, ReadResB2, !IO),
            ( if
                ReadResB = ok,
                BytesReadB = 3,
                ReadResB2 = ok,
                BytesReadB2 = 0,
                BMb ^ bits(16, 24) = InputBMb ^ bits(0, 24)
            then
                io.write_string("Second read succeeded\n", !IO)
            else
                io.write_string("Second read failed\n", !IO)
            ),
            io.close_binary_input(IStream, !IO),
            io.remove_file(FileName, _, !IO)
        ;
            OpenInputRes = error(Error),
            throw(Error)
        )
    ;
        OpenRes = error(Error),
        throw(Error)
    ).

:- pred test_text_io(io::di, io::uo) is det.

test_text_io(!IO) :-
    FileName = "bitmap_test_output2",
    BMa0 = bitmap.init(64, yes),
    BMa = BMa0 ^ bits(32, 16) := 0b1011011100100101,
    BMb0 = bitmap.init(47, yes),
    BMb = BMb0 ^ bits(11, 16) := 0b1011010110100101,
    io.write_string("BMa = ", !IO),
    io.write(BMa, !IO),
    io.write_string(".\n", !IO),
    io.write_string("BMb = ", !IO),
    io.write(BMb, !IO),
    io.write_string(".\n", !IO),
    io.open_output(FileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),

        io.write(Stream, BMa, !IO),
        io.write_string(Stream, ".\n", !IO),
        io.write(Stream, BMb, !IO),
        io.write_string(Stream, ".\n", !IO),
        io.close_output(Stream, !IO),
        io.open_input(FileName, OpenInputRes, !IO),
        (
            OpenInputRes = ok(IStream),
            io.read(IStream, ReadResA, !IO),
            ( if ReadResA = ok(BMa) then
                io.write_string("First read succeeded\n", !IO)
            else
                io.write_string("First read failed\n", !IO),
                io.close_input(IStream, !IO),
                throw(ReadResA)
            ),
            io.read(IStream, ReadResB, !IO),
            ( if ReadResB = ok(BMb) then
                io.write_string("Second read succeeded\n", !IO)
            else
                io.write_string("Second read failed\n", !IO),
                io.close_input(IStream, !IO),
                throw(ReadResB)
            ),
            io.close_input(IStream, !IO),
            io.remove_file(FileName, _, !IO)
        ;
            OpenInputRes = error(Error),
            throw(Error)
        )
    ;
        OpenRes = error(Error),
        throw(Error)
    ).

:- pred fill_in_alternating_pattern(byte::in,
            tbitmap::tbitmap_di, tbitmap::tbitmap_uo) is det.

fill_in_alternating_pattern(Byte, !BM) :-
    NumBits = !.BM ^ fst ^ num_bits,
    NumBytes = NumBits / bits_per_byte,
    fill_in_alternating_pattern(0, NumBytes, Byte, !BM),
    LastByteNumBits = NumBits `rem` bits_per_byte,
    ( if LastByteNumBits \= 0 then
        !:BM = !.BM ^ bits(NumBytes * bits_per_byte, LastByteNumBits) := Byte
    else
        true
    ).

:- pred fill_in_alternating_pattern(byte_index::in, num_bytes::in, byte::in,
            tbitmap::tbitmap_di, tbitmap::tbitmap_uo) is det.

fill_in_alternating_pattern(Index, NumBytes, Pattern, !BM) :-
    ( if Index >= NumBytes then
        true
    else
        ( if Index rem 2 = 0 then
            BytePattern = Pattern
        else
            BytePattern = \Pattern
        ),
        !:BM = !.BM ^ byte(Index) := BytePattern,
        fill_in_alternating_pattern(Index + 1, NumBytes, Pattern, !BM)
    ).

:- pred write_binary_string(word::in, io::di, io::uo) is det.

write_binary_string(Int, !IO) :-
    io.write_string(binary_string(Int), !IO).

:- func binary_string(word) = string.

binary_string(Int) = string.int_to_base_string(Int, 2).

:- pred write_bitmap_result_error(bitmap_result_error::in,
    io::di, io::uo) is det.

write_bitmap_result_error(query(Op, Input, OtherArgs, Output), !IO) :-
    io.write_string("Error in `", !IO),
    io.write_string(Op, !IO),
    io.write_string("(", !IO),
    io.write(OtherArgs, !IO),
    io.write_string(")'\ninput bitmap: ", !IO),
    io.write_string(to_byte_string(Input), !IO),
    io.nl(!IO),
    io.write_string("output = ", !IO),
    io.write(Output ^ fst, !IO),
    io.nl(!IO),
    io.write_string("expected output = ", !IO),
    io.write(Output ^ snd, !IO),
    io.nl(!IO).

write_bitmap_result_error(binary_query(Op, Input1, Input2, OtherArgs, Output),
        !IO) :-
    io.write_string("Error in `", !IO),
    io.write_string(Op, !IO),
    io.write_string("(", !IO),
    io.write(OtherArgs, !IO),
    io.write_string(")'\ninput bitmap 1: ", !IO),
    io.write_string(to_byte_string(Input1), !IO),
    io.write_string("\ninput bitmap 2: ", !IO),
    io.write_string(to_byte_string(Input2), !IO),
    io.nl(!IO),
    io.write_string("output = ", !IO),
    io.write(Output ^ fst, !IO),
    io.nl(!IO),
    io.write_string("expected output = ", !IO),
    io.write(Output ^ snd, !IO),
    io.nl(!IO).

write_bitmap_result_error(one_argument(Op, Input, OtherArgs, Output), !IO) :-
    io.write_string("Error in `", !IO),
    io.write_string(Op, !IO),
    io.write_string("(", !IO),
    io.write(OtherArgs, !IO),
    io.write_string(")'\ninput bitmap: ", !IO),
    io.write_string(to_byte_string(Input), !IO),
    io.nl(!IO),
    io.write_string("output = ", !IO),
    io.write_string(to_byte_string(Output ^ fst), !IO),
    io.nl(!IO),
    io.write_string("expected output = ", !IO),
    io.write_string(to_byte_string(Output ^ snd), !IO),
    io.nl(!IO).

write_bitmap_result_error(two_arguments(Op, Input1, Input2, OtherArgs, Output),
        !IO) :-
    io.write_string("Error in `", !IO),
    io.write_string(Op, !IO),
    io.write_string("(", !IO),
    io.write(OtherArgs, !IO),
    io.write_string(")'\ninput bitmap 1: ", !IO),
    io.write_string(to_byte_string(Input1), !IO),
    io.nl(!IO),
    io.write_string("\ninput bitmap 2: ", !IO),
    io.write_string(to_byte_string(Input2), !IO),
    io.nl(!IO),
    io.write_string("output = ", !IO),
    io.write_string(to_byte_string(Output ^ fst), !IO),
    io.nl(!IO),
    io.write_string("expected output = ", !IO),
    io.write_string(to_byte_string(Output ^ snd), !IO),
    io.nl(!IO).
