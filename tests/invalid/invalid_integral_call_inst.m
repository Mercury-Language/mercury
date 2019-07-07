%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check the error messages we generate when a call has vars with invalid
% instantiation states involving integer literals that need suffixes.
%

:- module invalid_integral_call_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    main_i08(!IO),
    main_i16(!IO),
    main_i32(!IO),
    main_i64(!IO),
    main_i(!IO),
    main_u08(!IO),
    main_u16(!IO),
    main_u32(!IO),
    main_u64(!IO),
    main_u(!IO).

%---------------------------------------------------------------------------%

:- pred main_i08(io::di, io::uo) is det.

main_i08(!IO) :-
    NI08 = 7i8,
    XI08 = test_int08(NI08),
    io.print_line(XI08, !IO).

:- pred main_i16(io::di, io::uo) is det.

main_i16(!IO) :-
    NI16 = 7i16,
    XI16 = test_int16(NI16),
    io.print_line(XI16, !IO).

:- pred main_i32(io::di, io::uo) is det.

main_i32(!IO) :-
    NI32 = 7i32,
    XI32 = test_int32(NI32),
    io.print_line(XI32, !IO).

:- pred main_i64(io::di, io::uo) is det.

main_i64(!IO) :-
    NI64 = 7i64,
    XI64 = test_int64(NI64),
    io.print_line(XI64, !IO).

:- pred main_i(io::di, io::uo) is det.

main_i(!IO) :-
    NI = 7,
    XI = test_int(NI),
    io.print_line(XI, !IO).

:- pred main_u08(io::di, io::uo) is det.

main_u08(!IO) :-
    NU08 = 7u8,
    XU08 = test_uint08(NU08),
    io.print_line(XU08, !IO).

:- pred main_u16(io::di, io::uo) is det.

main_u16(!IO) :-
    NU16 = 7u16,
    XU16 = test_uint16(NU16),
    io.print_line(XU16, !IO).

:- pred main_u32(io::di, io::uo) is det.

main_u32(!IO) :-
    NU32 = 7u32,
    XU32 = test_uint32(NU32),
    io.print_line(XU32, !IO).

:- pred main_u64(io::di, io::uo) is det.

main_u64(!IO) :-
    NU64 = 7u64,
    XU64 = test_uint64(NU64),
    io.print_line(XU64, !IO).

:- pred main_u(io::di, io::uo) is det.

main_u(!IO) :-
    NU = 7u,
    XU = test_uint(NU),
    io.print_line(XU, !IO).

%---------------------------------------------------------------------------%

:- func test_int08(int8::in(bound(2i8 ; 4i8 ; 6i8))) = (int8::out) is det.

test_int08(2i8) = 4i8.
test_int08(4i8) = 9i8.
test_int08(6i8) = 12i8.

:- func test_int16(int16::in(bound(2i16 ; 4i16 ; 6i16))) = (int16::out) is det.

test_int16(2i16) = 4i16.
test_int16(4i16) = 9i16.
test_int16(6i16) = 12i16.

:- func test_int32(int32::in(bound(2i32 ; 4i32 ; 6i32))) = (int32::out) is det.

test_int32(2i32) = 4i32.
test_int32(4i32) = 9i32.
test_int32(6i32) = 12i32.

:- func test_int64(int64::in(bound(2i64 ; 4i64 ; 6i64))) = (int64::out) is det.

test_int64(2i64) = 4i64.
test_int64(4i64) = 9i64.
test_int64(6i64) = 12i64.

:- func test_int(int::in(bound(2 ; 4 ; 6))) = (int::out) is det.

test_int(2) = 4.
test_int(4) = 9.
test_int(6) = 12.

:- func test_uint08(uint8::in(bound(2u8 ; 4u8 ; 6u8))) = (uint8::out) is det.

test_uint08(2u8) = 4u8.
test_uint08(4u8) = 9u8.
test_uint08(6u8) = 12u8.

:- func test_uint16(uint16::in(bound(2u16 ; 4u16 ; 6u16))) = (uint16::out)
    is det.

test_uint16(2u16) = 4u16.
test_uint16(4u16) = 9u16.
test_uint16(6u16) = 12u16.

:- func test_uint32(uint32::in(bound(2u32 ; 4u32 ; 6u32))) = (uint32::out)
    is det.

test_uint32(2u32) = 4u32.
test_uint32(4u32) = 9u32.
test_uint32(6u32) = 12u32.

:- func test_uint64(uint64::in(bound(2u64 ; 4u64 ; 6u64))) = (uint64::out)
    is det.

test_uint64(2u64) = 4u64.
test_uint64(4u64) = 9u64.
test_uint64(6u64) = 12u64.

:- func test_uint(uint::in(bound(2u ; 4u ; 6u))) = (uint::out) is det.

test_uint(2u) = 4u.
test_uint(4u) = 9u.
test_uint(6u) = 12u.
