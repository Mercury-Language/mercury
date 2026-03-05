%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 unsigned 32-bit integers.

:- module clamp_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module uint32.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(uint32)::in, pair(uint32)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(uint32)::in, uint32::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = uint32.clamp(Min, Max, N)
    then
        ResultStr = uint32_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%u, %u, %u) = %s\n",
        [u32(Min), u32(Max), u32(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint32).

numbers = [
    0_u32,
    1_u32,
    2_u32,
    8_u32,
    10_u32,
    16_u32,
    255_u32,
    65_535_u32,
    2_147_483_647_u32,
    4_294_967_295_u32
].

:- func ranges = list(pair(uint32)).

ranges = [
    0u32  - 0u32,
    0u32  - 5u32,
    5u32  - 10u32,
    0u32  - 255u32,
    12u32 - 6u32
].

%---------------------------------------------------------------------------%
:- end_module clamp_uint32.
%---------------------------------------------------------------------------%
