%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 signed 32-bit integers.

:- module clamp_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int32.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(int32)::in, pair(int32)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(int32)::in, int32::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = int32.clamp(Min, Max, N)
    then
        ResultStr = int32_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%d, %d, %d) = %s\n",
        [i32(Min), i32(Max), i32(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int32).

numbers = [
    -2_147_483_648_i32,
    -32_768_i32,
    -128_i32,
    0_i32,
    1_i32,
    2_i32,
    8_i32,
    10_i32,
    16_i32,
    127_i32,
    32_767_i32,
    2_147_483_647_i32
].

:- func ranges = list(pair(int32)).

ranges = [
    -2i32  - 12i32,
    -10i32 - 5i32,
    0i32   - 0i32,
    -1i32  - 1i32,
    5i32   - 10i32,
    12i32  - 6i32
].

%---------------------------------------------------------------------------%
:- end_module clamp_int32.
%---------------------------------------------------------------------------%
