%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test conversion of uints to strings.
% The .exp file is from systems where uint is 32 bit.
% The .exp2 file is for systems wwhere uint is 64 bit.
%---------------------------------------------------------------------------%

:- module uint_string_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint.

main(!IO) :-
    io.format("%-22s %-24s %-22s%-22s\n",
        [s("Decimal"), s("Octal"), s("Hex"), s("HEX")], !IO),
    list.foldl(do_test, test_values, !IO),
    io.nl(!IO).

:- pred do_test( uint, io, io).
:- mode do_test(in, di, uo) is det.

do_test(U, !IO) :-
    Decimal = uint_to_string(U),
    Octal = uint_to_octal_string(U),
    Hex = uint_to_hex_string(U),
    HexUC = uint_to_uc_hex_string(U),
    io.format("%-22s %-24s %-22s%-22s\n",
        [s(Decimal), s(Octal), s(Hex), s(HexUC)], !IO).

:- func test_values = list(uint).

test_values = [
   0u,
   1u,
   2u,
   3u,
   4u,
   7u,
   8u,
   9u,
   10u,
   11u,
   12u,
   13u,
   14u,
   15u,
   16u,
   32u,
   64u,
   127u,
   128u,
   255u,
   256u,
   32767u,
   65535u,
   2147483647u,
   4294967295u,
   uint.max_uint
].
