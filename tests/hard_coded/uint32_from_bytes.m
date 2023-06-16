%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module uint32_from_bytes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module uint32.

main(!IO) :-
     Test1 = uint32.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0xffu8),
     io.write_line(Test1, !IO),
     Test2 = uint32.from_bytes_le(0x00u8, 0x00u8, 0xffu8, 0x00u8),
     io.write_line(Test2, !IO),
     Test3 = uint32.from_bytes_le(0x00u8, 0xffu8, 0x00u8, 0x00u8),
     io.write_line(Test3, !IO),
     Test4 = uint32.from_bytes_le(0xffu8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test4, !IO),

     Test5 = uint32.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0xffu8),
     io.write_line(Test5, !IO),
     Test6 = uint32.from_bytes_be(0x00u8, 0x00u8, 0xffu8, 0x00u8),
     io.write_line(Test6, !IO),
     Test7 = uint32.from_bytes_be(0x00u8, 0xffu8, 0x00u8, 0x00u8),
     io.write_line(Test7, !IO),
     Test8 = uint32.from_bytes_be(0xffu8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test8, !IO).
