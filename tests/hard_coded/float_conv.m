%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module float_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing int8 -> float conversion ***\n\n", !IO),
    run_test("from_int8", float.from_int8, int8s, !IO),
    io.write_string("\n*** Testing uint8 -> float conversion ***\n\n", !IO),
    run_test("from_uint8", float.from_uint8, uint8s, !IO),
    io.write_string("\n*** Testing int16 -> float conversion ***\n\n", !IO),
    run_test("from_int16", float.from_int16, int16s, !IO),
    io.write_string("\n*** Testing uint16 -> float conversion ***\n\n", !IO),
    run_test("from_uint16", float.from_uint16, uint16s, !IO).

:- pred run_test(string::in, (func(T) = float)::in, list(T)::in,
    io::di, io::uo) is det.

run_test(ConvDesc, ConvFunc, Ints, !IO) :-
    list.foldl(do_test(ConvDesc, ConvFunc), Ints, !IO).

:- pred do_test(string::in, (func(T) = float)::in, T::in, io::di, io::uo) is det.

do_test(ConvDesc, ConvFunc, Int, !IO) :-
    io.write_string(ConvDesc ++ "(", !IO),
    io.write(Int, !IO),
    io.write_string(") = ", !IO),
    Float = ConvFunc(Int),
    io.write_line(Float, !IO).

:- func int8s = list(int8).

int8s = [
    -128i8,
    -1i8,
    0i8,
    8i8,
    16i8,
    32i8,
    64i8,
    127i8
].

:- func uint8s = list(uint8).

uint8s = [
   0u8,
   1u8,
   2u8,
   8u8,
   23u8,
   127u8,
   128u8,
   255u8
].

:- func int16s = list(int16).

int16s = [
    -32768i16,
    -129i16,
    -128i16,
    -1i16,
    0i16,
    1i16,
    127i16,
    128i16,
    32767i16
].

:- func uint16s = list(uint16).

uint16s = [
    0u16,
    1u16,
    8u16,
    127u16,
    128u16,
    32767u16
].

