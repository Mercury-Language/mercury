%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uint32s -> uint64s.

:- module uint32_to_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(uint32::in, io::di, io::uo) is det.

do_test(U32, !IO) :-
    U64 = cast_to_uint64(U32),
    io.format("cast_to_uint64(%su32) = %su64\n",
        [s(uint32_to_string(U32)), s(uint64_to_string(U64))], !IO).

:- func numbers = list(uint32).

numbers = [
    0u32,
    1u32,
    2u32,
    8u32,
    10u32,
    16u32,
    127u32,
    32767u32,
    2147483647u32,
    2147483648u32,
    4294967295u32
].

%---------------------------------------------------------------------------%
:- end_module uint32_to_uint64.
%---------------------------------------------------------------------------%
