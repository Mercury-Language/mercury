%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test conversion of int64, uint64s and uint32s to ints.
% These are the sized types whose conversion to an int
% may result in a value that the int type cannot accurately represent.
%
% The .exp  file is for platforms where an int is 64 bits.
% The .exp2 file is for platforms where an int is 32 bits.
%

:- module truncate_to_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test_i64, numbers_i64, !IO),
    io.nl(!IO),
    list.foldl(test_u64, numbers_u64, !IO),
    io.nl(!IO),
    list.foldl(test_u32, numbers_u32, !IO).

%---------------------------------------------------------------------------%

:- func numbers_i64 = list(int64).

numbers_i64 = [
    -9223372036854775807_i64,
    -4294967296_i64,
    -2147483649_i64,
    -2147483648_i64,
    -2147483647_i64,
    -64767_i64,
    -1_i64,
    0_i64,
    1_i64,
    2_i64,
    8_i64,
    10_i64,
    16_i64,
    127_i64,
    64767_i64,
    2147483647_i64,
    2147483648_i64,
    2147483649_i64,
    4294967295_i64,
    4294967296_i64,
    9223372036854775807_i64
].

:- func numbers_u64 = list(uint64).

numbers_u64 = [
    0_u64,
    1_u64,
    2_u64,
    8_u64,
    10_u64,
    16_u64,
    127_u64,
    64767_u64,
    2147483647_u64,
    2147483648_u64,
    4294967295_u64,
    4294967296_u64,
    9223372036854775807_u64,
    9223372036854775808_u64,
    10223372036854775807_u64
].

:- func numbers_u32 = list(uint32).

numbers_u32 = [
    0_u32,
    1_u32,
    2_u32,
    8_u32,
    10_u32,
    16_u32,
    127_u32,
    32767_u32,
    2147483647_u32,
    2147483648_u32,
    4294967295_u32
].

%---------------------------------------------------------------------------%

:- pred test_i64(int64::in, io::di, io::uo) is det.

test_i64(I64, !IO) :-
    ( if int64_to_int(I64, I) then
        io.format("int64.to_int(%20d %16x) = %20d\n",
            [i64(I64), i64(I64), i(I)], !IO)
    else
        io.format("int64.to_int(%20d %16x)   %20s\n",
            [i64(I64), i64(I64), s("failed")], !IO)
    ).

:- pred test_u64(uint64::in, io::di, io::uo) is det.

test_u64(U64, !IO) :-
    ( if uint64_to_int(U64, I) then
        io.format("uint64.to_int(%16x) = %16x\n", [u64(U64), i(I)], !IO)
    else
        io.format("uint64.to_int(%16x)   %16s\n", [u64(U64), s("failed")], !IO)
    ).

:- pred test_u32(uint32::in, io::di, io::uo) is det.

test_u32(U32, !IO) :-
    ( if uint32_to_int(U32, I) then
        io.format("uint32.to_int(%8x) = %8x\n", [u32(U32), i(I)], !IO)
    else
        io.format("uint32.to_int(%8x)   %8s\n", [u32(U32), s("failed")], !IO)
    ).

%---------------------------------------------------------------------------%
% The following predicates are intended to be moved to int64.m, uint64,
% and uint32.m in the standard library, once they have C# and Java code.
%---------------------------------------------------------------------------%

:- pred int64_to_int(int64::in, int::out) is semidet.

:- pragma foreign_proc("C",
    int64_to_int(I64::in, I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#if MR_BYTES_PER_WORD == 8
    // Every bit in I64 means the same in I.
    I = (MR_Integer) I64;
    SUCCESS_INDICATOR = MR_TRUE;
#else
    // This is the code of int32.from_int, but with different types.
    //
    // In int32.from_int,
    // - the input is an int, which may be 32 or 64 bits, and
    // - the output is an int32.
    //
    // Here,
    // - the input is an int64, and
    // - the output is an int, which happens to be 32 bits.
    //
    // So while the types are different, the bit counts are the same.
    if (I64 > (int64_t) INT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (I64 < (int64_t) INT32_MIN) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        I = (MR_Integer) I64;
        SUCCESS_INDICATOR = MR_TRUE;
    }
#endif
").

%---------------------------------------------------------------------------%

:- pred uint64_to_int(uint64::in, int::out) is semidet.

:- pragma foreign_proc("C",
    uint64_to_int(U64::in, I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    uint64_t    mask_for_int;

#if MR_BYTES_PER_WORD == 8
    mask_for_int = (1UL << 63) - 1;
#else
    mask_for_int = (1UL << 31) - 1;
#endif

    if ((U64 & (~mask_for_int)) == 0UL) {
        // Every bit in U64 means the same in I.
        I = (MR_Integer) U64;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        // Some bit in U64 does not mean the same in I.
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%---------------------------------------------------------------------------%

:- pred uint32_to_int(uint32::in, int::out) is semidet.

:- pragma foreign_proc("C",
    uint32_to_int(U32::in, I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#if MR_BYTES_PER_WORD == 8
    // Every bit in U32 means the same in I.
    I = (MR_Integer) U32;
    SUCCESS_INDICATOR = MR_TRUE;
#else
    uint32_t    mask_for_int;

    mask_for_int = (1UL << 31) - 1;
    if ((U32 & (~mask_for_int)) == 0UL) {
        // Every bit in U32 means the same in I.
        I = (MR_Integer) U32;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        // Some bit in U32 does not mean the same in I.
        SUCCESS_INDICATOR = MR_FALSE;
    }
#endif
").

%---------------------------------------------------------------------------%
:- end_module truncate_to_int.
%---------------------------------------------------------------------------%
