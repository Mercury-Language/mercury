%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Avoid generating tautological comparisons that the C compiler may detect.
%

:- module tautological_compare.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module int8.
:- import_module uint.
:- import_module uint8.

main(!IO) :-
    write_string("\nint:\n", !IO),
    test_int(1, !IO),
    write_string("\nuint:\n", !IO),
    test_uint(1u, !IO),

    write_string("\nint8:\n", !IO),
    test_int8(1i8, !IO),
    write_string("\nuint8:\n", !IO),
    test_uint8(1u8, !IO),

    write_string("\nint16:\n", !IO),
    test_int16(1i8, !IO),
    write_string("\nuint16:\n", !IO),
    test_uint16(1u8, !IO),

    write_string("\nint32:\n", !IO),
    test_int32(1i8, !IO),
    write_string("\nuint32:\n", !IO),
    test_uint32(1u8, !IO),

    write_string("\nint64:\n", !IO),
    test_int64(1i8, !IO),
    write_string("\nuint64:\n", !IO),
    test_uint64(1u8, !IO),

    write_string("\ncompare:\n", !IO),
    test_compare(!IO).

%---------------------------------------------------------------------------%

:- pred test_int(int::in, io::di, io::uo) is det.

test_int(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

:- pred test_uint(uint::in, io::di, io::uo) is det.

test_uint(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int8(int8::in, io::di, io::uo) is det.

test_int8(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

:- pred test_uint8(uint8::in, io::di, io::uo) is det.

test_uint8(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int16(int8::in, io::di, io::uo) is det.

test_int16(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

:- pred test_uint16(uint8::in, io::di, io::uo) is det.

test_uint16(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int32(int8::in, io::di, io::uo) is det.

test_int32(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

:- pred test_uint32(uint8::in, io::di, io::uo) is det.

test_uint32(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int64(int8::in, io::di, io::uo) is det.

test_int64(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

:- pred test_uint64(uint8::in, io::di, io::uo) is det.

test_uint64(I, !IO) :-
    ( if I < I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I > I then
        write_string("wrong\n", !IO)
    else
        write_string("ok\n", !IO)
    ),
    ( if I =< I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ),
    ( if I >= I then
        write_string("ok\n", !IO)
    else
        write_string("wrong\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_compare(io::di, io::uo) is det.

test_compare(!IO) :-
    I = 1,
    compare(RI, I, I),
    print_comparison(RI, !IO),
    U = 1u,
    compare(RU, U, U),
    print_comparison(RU, !IO),

    I8 = 1i8,
    compare(R8, I8, I8),
    print_comparison(R8, !IO),
    U8 = 1u8,
    compare(RU8, U8, U8),
    print_comparison(RU8, !IO),

    I16 = 1i16,
    compare(R16, I16, I16),
    print_comparison(R16, !IO),
    U16 = 1u16,
    compare(RU16, U16, U16),
    print_comparison(RU16, !IO),

    I32 = 1i32,
    compare(R32, I32, I32),
    print_comparison(R32, !IO),
    U32 = 1u32,
    compare(RU32, U32, U32),
    print_comparison(RU32, !IO),

    I64 = 1i64,
    compare(R64, I64, I64),
    print_comparison(R64, !IO),
    U64 = 1u64,
    compare(RU64, U64, U64),
    print_comparison(RU64, !IO).

:- pred print_comparison(comparison_result::in, io::di, io::uo) is det.

print_comparison(R, !IO) :-
    (
        R = (=),
        write_string("ok\n", !IO)
    ;
        ( R = (<)
        ; R = (>)
        ),
        write_string("wrong\n", !IO)
    ).

%---------------------------------------------------------------------------%
