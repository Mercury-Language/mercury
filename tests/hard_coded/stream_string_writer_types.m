%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
%
% Check that the types whose treatment we specialize in stream.string_writer.m
% have the expected qualified names.
%

:- module stream_string_writer_types.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bitmap.
:- import_module bool.
:- import_module calendar.
:- import_module integer.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

main(!IO) :-
    Ptr = get_c_pointer,
    BitMap = bitmap.init(10, no),
    IntegerOne = integer.one,
    type_to_univ("xyzzy", Univ),
    Date = det_init_date(2019, october, 31, 12, 0, 0, 0),
    Duration = init_duration(1, 2, 3, 4, 5, 6, 7),
    TypeDesc = type_of(1),
    TypeCtorDesc = type_ctor(TypeDesc),
    io.input_stream(TextInputStream, !IO),
    io.output_stream(TextOutputStream, !IO),
    io.binary_input_stream(BinaryInputStream, !IO),
    io.binary_output_stream(BinaryOutputStream, !IO),

    write_type_ctor_of("xyz", !IO),
    write_type_ctor_of('q', !IO),
    write_type_ctor_of(42, !IO),
    write_type_ctor_of(43u, !IO),
    write_type_ctor_of(52i8, !IO),
    write_type_ctor_of(53u8, !IO),
    write_type_ctor_of(62i16, !IO),
    write_type_ctor_of(63u16, !IO),
    write_type_ctor_of(72i32, !IO),
    write_type_ctor_of(73u32, !IO),
    write_type_ctor_of(82i64, !IO),
    write_type_ctor_of(83u64, !IO),
    write_type_ctor_of(1.23, !IO),
    write_type_ctor_of(Ptr, !IO),
    write_type_ctor_of(BitMap, !IO),
    write_type_ctor_of(IntegerOne, !IO),
    write_type_ctor_of(Univ, !IO),
    write_type_ctor_of(Date, !IO),
    write_type_ctor_of(Duration, !IO),
    write_type_ctor_of(TypeDesc, !IO),
    write_type_ctor_of(TypeCtorDesc, !IO),
    write_type_ctor_of(TextInputStream, !IO),
    write_type_ctor_of(TextOutputStream, !IO),
    write_type_ctor_of(BinaryInputStream, !IO),
    write_type_ctor_of(BinaryOutputStream, !IO).

:- pred write_type_ctor_of(T::in, io::di, io::uo) is det.

write_type_ctor_of(X, !IO) :-
    TypeDesc = type_of(X),
    TypeCtorDesc = type_ctor(TypeDesc),
    type_ctor_name_and_arity(TypeCtorDesc,
        TypeCtorModuleName, TypeCtorName, TypeCtorArity),
    io.format("module %s, name %s, arity %d\n",
        [s(TypeCtorModuleName), s(TypeCtorName), i(TypeCtorArity)], !IO).

:- func get_c_pointer = c_pointer.

:- pragma foreign_proc("C",
    get_c_pointer = (Ptr::out),
    [promise_pure, thread_safe],
"
    Ptr = 0;
").
