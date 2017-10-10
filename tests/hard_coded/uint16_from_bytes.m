:- module uint16_from_bytes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module uint16.

main(!IO) :-
     Test1 = uint16.from_bytes_le(0x00u8, 0xffu8),
     io.write_line(Test1, !IO),
     Test2 = uint16.from_bytes_le(0xffu8, 0x00u8),
     io.write_line(Test2, !IO),
     Test3 = uint16.from_bytes_be(0x00u8, 0xffu8),
     io.write_line(Test3, !IO),
     Test4 = uint16.from_bytes_be(0xffu8, 0x00u8),
     io.write_line(Test4, !IO).
