%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 signed 16-bit integers.

:- module clamp_int16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int16.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(int16)::in, pair(int16)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(int16)::in, int16::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = int16.clamp(Min, Max, N)
    then
        ResultStr = int16_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%d, %d, %d) = %s\n",
        [i16(Min), i16(Max), i16(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int16).

numbers = [
    -32_768_i16,
    -128_i16,
    0_i16,
    1_i16,
    2_i16,
    8_i16,
    10_i16,
    16_i16,
    127_i16,
    255_i16,
    32_767_i16
].

:- func ranges = list(pair(int16)).

ranges = [
    -2i16  - 12i16,
    -10i16 - 5i16,
    0i16   - 0i16,
    -1i16  - 1i16,
    5i16   - 10i16,
    12i16  - 6i16
].

%---------------------------------------------------------------------------%
:- end_module clamp_int16.
%---------------------------------------------------------------------------%
