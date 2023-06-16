%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module uint64_from_bytes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module uint64.

main(!IO) :-
     Test1 = uint64.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0xffu8),
     io.write_line(Test1, !IO),
     Test2 = uint64.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0xffu8, 0x00u8),
     io.write_line(Test2, !IO),
     Test3 = uint64.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0xffu8, 0x00u8, 0x00u8),
     io.write_line(Test3, !IO),
     Test4 = uint64.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0xffu8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test4, !IO),
     Test5 = uint64.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0xffu8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test5, !IO),
     Test6 = uint64.from_bytes_le(0x00u8, 0x00u8, 0xffu8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test6, !IO),
     Test7 = uint64.from_bytes_le(0x00u8, 0xffu8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test7, !IO),
     Test8 = uint64.from_bytes_le(0xffu8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test8, !IO),

     Test9 = uint64.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0xffu8),
     io.write_line(Test9, !IO),
     Test10 = uint64.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0xffu8, 0x00u8),
     io.write_line(Test10, !IO),
     Test11 = uint64.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0xffu8, 0x00u8, 0x00u8),
     io.write_line(Test11, !IO),
     Test12 = uint64.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0x00u8,
        0xffu8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test12, !IO),
     Test13 = uint64.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0xffu8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test13, !IO),
     Test14 = uint64.from_bytes_be(0x00u8, 0x00u8, 0xffu8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test14, !IO),
     Test15 = uint64.from_bytes_be(0x00u8, 0xffu8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test15, !IO),
     Test16 = uint64.from_bytes_be(0xffu8, 0x00u8, 0x00u8, 0x00u8,
        0x00u8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test16, !IO).
