:- module int32_from_bytes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int32.

main(!IO) :-
     Test1 = int32.from_bytes_le(0x00u8, 0x00u8, 0x00u8, 0xffu8),
     io.write_line(Test1, !IO),
     Test2 = int32.from_bytes_le(0x00u8, 0x00u8, 0xffu8, 0x00u8),
     io.write_line(Test2, !IO),
     Test3 = int32.from_bytes_le(0x00u8, 0xffu8, 0x00u8, 0x00u8),
     io.write_line(Test3, !IO),
     Test4 = int32.from_bytes_le(0xffu8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test4, !IO),

     Test5 = int32.from_bytes_be(0x00u8, 0x00u8, 0x00u8, 0xffu8),
     io.write_line(Test5, !IO),
     Test6 = int32.from_bytes_be(0x00u8, 0x00u8, 0xffu8, 0x00u8),
     io.write_line(Test6, !IO),
     Test7 = int32.from_bytes_be(0x00u8, 0xffu8, 0x00u8, 0x00u8),
     io.write_line(Test7, !IO),
     Test8 = int32.from_bytes_be(0xffu8, 0x00u8, 0x00u8, 0x00u8),
     io.write_line(Test8, !IO).
