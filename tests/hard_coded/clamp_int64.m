%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 signed 64-bit integers.

:- module clamp_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int64.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(int64)::in, pair(int64)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(int64)::in, int64::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = int64.clamp(Min, Max, N)
    then
        ResultStr = int64_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%d, %d, %d) = %s\n",
        [i64(Min), i64(Max), i64(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int64).

numbers = [
    -9_223_372_036_854_775_808_i64,
    -2_147_483_648_i64,
    -64_768_i64,
    -128_i64,
    0_i64,
    1_i64,
    2_i64,
    8_i64,
    10_i64,
    16_i64,
    127_i64,
    64_767_i64,
    2_147_483_647_i64,
    9_223_372_036_854_775_807_i64
].

:- func ranges = list(pair(int64)).

ranges = [
    -2i64  - 12i64,
    -10i64 - 5i64,
    0i64   - 0i64,
    -1i64  - 1i64,
    5i64   - 10i64,
    12i64  - 6i64
].

%---------------------------------------------------------------------------%
:- end_module clamp_int64.
%---------------------------------------------------------------------------%
