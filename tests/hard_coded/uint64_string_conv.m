%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test conversion of uint64s to strings.
%---------------------------------------------------------------------------%

:- module uint64_string_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint64.

main(!IO) :-
    io.format("%-22s %-24s %-22s%-22s\n",
        [s("Decimal"), s("Octal"), s("Hex"), s("HEX")], !IO),
    list.foldl(do_test, test_values, !IO),
    io.nl(!IO).

:- pred do_test(uint64::in, io::di, io::uo) is det.

do_test(U, !IO) :-
    Decimal = uint64_to_string(U),
    Octal = uint64_to_octal_string(U),
    Hex = uint64_to_hex_string(U),
    HexUC = uint64_to_uc_hex_string(U),
    io.format("%-22s %-24s %-22s%-22s\n",
        [s(Decimal), s(Octal), s(Hex), s(HexUC)], !IO).

:- func test_values = list(uint64).

test_values = [
   0u64,
   1u64,
   2u64,
   3u64,
   4u64,
   7u64,
   8u64,
   9u64,
   10u64,
   11u64,
   12u64,
   13u64,
   14u64,
   15u64,
   16u64,
   32u64,
   64u64,
   127u64,
   128u64,
   255u64,
   256u64,
   32767u64,
   65535u64,
   2147483647u64,
   4294967295u64,
   9223372036854775807u64,
   18446744073709551615u64
].
