%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 unsigned 8-bit integers.

:- module clamp_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module uint8.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(uint8)::in, pair(uint8)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(uint8)::in, uint8::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = uint8.clamp(Min, Max, N)
    then
        ResultStr = uint8_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%u, %u, %u) = %s\n",
        [u8(Min), u8(Max), u8(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint8).

numbers = [
    0u8,
    1u8,
    2u8,
    8u8,
    10u8,
    16u8,
    127u8,
    128u8,
    255u8
].

:- func ranges = list(pair(uint8)).

ranges = [
    0u8 - 0u8,
    0u8 - 5u8,
    5u8 - 10u8,
    0u8 - 255u8,
    12u8 - 6u8
].

%---------------------------------------------------------------------------%
:- end_module clamp_uint8.
%---------------------------------------------------------------------------%
