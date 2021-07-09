%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test conversion of uints to unsigned 64-bit integers.
%
% The .exp file is for when int is 64-bit.
% The .exp2 file is for when int is 32-bit.
%
%---------------------------------------------------------------------------%

:- module from_uint_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(string::in, io::di, io::uo) is det.

do_test(UIntStr, !IO) :-
    io.format("from_uint(%s) = ", [s(UIntStr)], !IO),
    ( if
        string.to_uint(UIntStr, UInt),
        UInt64 = uint64.cast_from_uint(UInt)
    then
        io.format("%u\n", [u64(UInt64)], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(string).

numbers = [
    "0",
    "1",
    "2",
    "8",
    "10",
    "16",
    "127",
    "32767",
    "2147483647",          % INT32_MAX
    "2147483648",          % INT32_MAX + 1
    "4294967295",          % UINT32_MAX
    "4294967296",          % UINT32_MAX + 1
    "9223372036854775807", % INT64_MAX
    "9223372036854775808", % INT64_MAX + 1
    "18446744073709551615" % UINT64_MAX
].

%---------------------------------------------------------------------------%
:- end_module from_uint_uint64.
%---------------------------------------------------------------------------%
