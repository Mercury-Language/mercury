%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 unsigned 64-bit integers.

:- module clamp_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module uint64.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(uint64)::in, pair(uint64)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(uint64)::in, uint64::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = uint64.clamp(Min, Max, N)
    then
        ResultStr = uint64_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%u, %u, %u) = %s\n",
        [u64(Min), u64(Max), u64(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint64).

numbers = [
    0_u64,
    1_u64,
    2_u64,
    8_u64,
    10_u64,
    16_u64,
    255_u64,
    65_535_u64,
    2_147_483_647_u64,
    4_294_967_295_u64,
    18_446_744_073_709_551_615_u64
].

:- func ranges = list(pair(uint64)).

ranges = [
    0u64  - 0u64,
    0u64  - 5u64,
    5u64  - 10u64,
    0u64  - 255u64,
    12u64 - 6u64
].

%---------------------------------------------------------------------------%
:- end_module clamp_uint64.
%---------------------------------------------------------------------------%
