%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pack.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- type dummy
    --->    dummy.

:- type fruit
    --->    apple
    ;       pear
    ;       orange.

:- type test_dummy
    --->    test_dummy(
                int,
                float,
                fruit,
                dummy,
                fruit,
                int,
                float
            ).

:- type test_i8
    --->    test_i8(
                int,
                float,
                fruit,
                int8,
                fruit,
                int,
                float
            ).

:- type test_u8
    --->    test_u8(
                int,
                float,
                fruit,
                uint8,
                fruit,
                int,
                float
            ).

:- type test_i64
    --->    test_i64(
                int,
                float,
                fruit,
                int64,
                fruit,
                int,
                float
            ).

:- type test_u64
    --->    test_u64(
                int,
                float,
                fruit,
                uint64,
                fruit,
                int,
                float
            ).

:- type test_abs(T)
    --->    test_abs(
                int,
                float,
                fruit,
                T,
                fruit,
                int,
                float
            ).

%---------------------------------------------------------------------------%

main(!IO) :-
    TestDummy = test_dummy(42, 99.9, pear, dummy, orange, 666, 1111.1),
    run_test_dummy(TestDummy, !IO),

    TestI8A = test_i8(42, 99.9, pear, -128i8, orange, 666, 1111.1),
    run_test_i8(TestI8A, !IO),
    TestI8B = test_i8(42, 99.9, pear, 127i8, orange, 666, 1111.1),
    run_test_i8(TestI8B, !IO),

    TestU8A = test_u8(42, 99.9, pear, 0u8, orange, 666, 1111.1),
    run_test_u8(TestU8A, !IO),
    TestU8B = test_u8(42, 99.9, pear, 255u8, orange, 666, 1111.1),
    run_test_u8(TestU8B, !IO),

    TestI64A = test_i64(42, 99.9, pear, -2525252525i64, orange, 666, 1111.1),
    run_test_i64(TestI64A, !IO),
    TestI64B = test_i64(42, 99.9, pear, 2525252525i64, orange, 666, 1111.1),
    run_test_i64(TestI64B, !IO),

    TestU64A = test_u64(42, 99.9, pear, 25u64, orange, 666, 1111.1),
    run_test_u64(TestU64A, !IO),
    TestU64B = test_u64(42, 99.9, pear, 2525252525u64, orange, 666, 1111.1),
    run_test_u64(TestU64B, !IO),

    AbsDummy = test_abs(42, 99.9, pear, dummy, orange, 666, 1111.1),
    run_test_abs(AbsDummy, !IO),

    AbsI8A = test_abs(42, 99.9, pear, -128i8, orange, 666, 1111.1),
    run_test_abs(AbsI8A, !IO),
    AbsI8B = test_abs(42, 99.9, pear, 127i8, orange, 666, 1111.1),
    run_test_abs(AbsI8B, !IO),

    AbsU8A = test_abs(42, 99.9, pear, 0u8, orange, 666, 1111.1),
    run_test_abs(AbsU8A, !IO),
    AbsU8B = test_abs(42, 99.9, pear, 255u8, orange, 666, 1111.1),
    run_test_abs(AbsU8B, !IO),

    AbsI64A = test_abs(42, 99.9, pear, -2525252525i64, orange, 666, 1111.1),
    run_test_abs(AbsI64A, !IO),
    AbsI64B = test_abs(42, 99.9, pear, 2525252525i64, orange, 666, 1111.1),
    run_test_abs(AbsI64B, !IO),

    AbsU64A = test_abs(42, 99.9, pear, 25u64, orange, 666, 1111.1),
    run_test_abs(AbsU64A, !IO),
    AbsU64B = test_abs(42, 99.9, pear, 2525252525u64, orange, 666, 1111.1),
    run_test_abs(AbsU64B, !IO).

%---------------------------------------------------------------------------%

:- pred run_test_dummy(test_dummy::in, io::di, io::uo) is det.

run_test_dummy(Term, !IO) :-
    io.write_line(Term, !IO).

:- pred run_test_i8(test_i8::in, io::di, io::uo) is det.

run_test_i8(Term, !IO) :-
    io.write_line(Term, !IO).

:- pred run_test_u8(test_u8::in, io::di, io::uo) is det.

run_test_u8(Term, !IO) :-
    io.write_line(Term, !IO).

:- pred run_test_i64(test_i64::in, io::di, io::uo) is det.

run_test_i64(Term, !IO) :-
    io.write_line(Term, !IO).

:- pred run_test_u64(test_u64::in, io::di, io::uo) is det.

run_test_u64(Term, !IO) :-
    io.write_line(Term, !IO).

:- pred run_test_abs(test_abs(T)::in, io::di, io::uo) is det.

run_test_abs(Term, !IO) :-
    io.write_line(Term, !IO).

%---------------------------------------------------------------------------%
