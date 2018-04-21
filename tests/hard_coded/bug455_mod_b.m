:- module bug455_mod_b.
:- interface.

:- func foo_int = int.
:- func foo_int8 = int8.
:- func foo_int16 = int16.
:- func foo_int32 = int32.
:- func foo_int64 = int64.
:- func foo_uint = uint.
:- func foo_uint8 = uint8.
:- func foo_uint16 = uint16.
:- func foo_uint32 = uint32.
:- func foo_uint64 = uint64.

:- implementation.

:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

foo_int = 0x1f << 1.
foo_int8 = 0x1f_i8 << 1.
foo_int16 = 0x1f_i16 << 1.
foo_int32 = 0x1f_i32 << 1.
foo_int64 = 0x1f_i64 << 1.

foo_uint = 0x1f_u << 1.
foo_uint8 = 0x1f_u8 << 1.
foo_uint16 = 0x1f_u16 << 1.
foo_uint32 = 0x1f_u32 << 1.
foo_uint64 = 0x1f_u64 << 1.
