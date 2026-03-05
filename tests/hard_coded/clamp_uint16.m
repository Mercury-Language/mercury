%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test clamp/3 unsigned 16-bit integers.

:- module clamp_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module uint16.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_clamp_test(numbers), ranges, !IO).

:- pred run_clamp_test(list(uint16)::in, pair(uint16)::in, io::di, io::uo)
    is cc_multi.

run_clamp_test(Numbers, Range, !IO) :-
    list.foldl(run_clamp_test_2(Range), Numbers, !IO).

:- pred run_clamp_test_2(pair(uint16)::in, uint16::in, io::di, io::uo)
    is cc_multi.

run_clamp_test_2(Range, N, !IO) :-
    Range = Min - Max,
    ( try []
        Result = uint16.clamp(Min, Max, N)
    then
        ResultStr = uint16_to_string(Result)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("clamp(%u, %u, %u) = %s\n",
        [u16(Min), u16(Max), u16(N), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint16).

numbers = [
    0_u16,
    1_u16,
    2_u16,
    8_u16,
    10_u16,
    16_u16,
    255_u16,
    65_535_u16
].

:- func ranges = list(pair(uint16)).

ranges = [
    0u16  - 0u16,
    0u16  - 5u16,
    5u16  - 10u16,
    0u16  - 255u16,
    12u16 - 6u16
].

%---------------------------------------------------------------------------%
:- end_module clamp_uint16.
%---------------------------------------------------------------------------%
